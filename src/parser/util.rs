use core::str;

pub mod peek_minivec {
    use std::mem::MaybeUninit;

    pub struct Peekable<I: Iterator, const N: usize> {
        len: usize,
        peek: [MaybeUninit<Option<I::Item>>; N],
        iter: I,
    }

    impl<I: Iterator, const N: usize> Drop for Peekable<I, N> {
        fn drop(&mut self) {
            while self.len > 0 {
                _ = self.next();
            }
        }
    }
    impl<I: Iterator, const N: usize> Peekable<I, N> {
        pub fn new(iter: I) -> Self {
            Self {
                len: 0,
                peek: [const { MaybeUninit::uninit() }; N],
                iter,
            }
        }
        pub fn peek<const INX: usize>(&mut self) -> Option<&I::Item> {
            const { assert!(INX < N) }
            while self.len <= INX {
                unsafe {
                    self.peek[self.len].as_mut_ptr().write(self.iter.next());
                }
                self.len += 1;
            }
            // at this point self.len is equal or greater than Inx so it is safe to index into the array this far
            unsafe { self.peek[INX].assume_init_ref() }.as_ref()
        }
    }

    impl<I: Iterator, const N: usize> std::iter::Iterator for Peekable<I, N> {
        type Item = I::Item;

        fn next(&mut self) -> Option<Self::Item> {
            if self.len > 0 {
                // the length is at least one so we can safely read the first element
                let first_out = unsafe { self.peek[0].as_mut_ptr().read() };

                self.len -= 1;
                // copy 1..len into 0..(len-1)
                // this only needs to be done when the remaining length is larger than 0 or the actual peekahead buffer size is larger than 1
                if self.len > 0 && N > 1 {
                    unsafe {
                        // we shift all outstanding elements down one
                        let dst = self.peek.as_mut_ptr();
                        let src = dst.add(1);
                        std::ptr::copy(src, dst, self.len);
                    }
                }

                first_out
            } else {
                self.iter.next()
            }
        }
    }

    #[test]
    pub fn weird() {
        let items = [1, 2, 3, 4, 5, 6, 7, 8].iter().copied();
        let mut peek0 = Peekable::<_, 0>::new(items.clone());
        assert_eq!(peek0.next(), Some(1));
        assert_eq!(peek0.next(), Some(2));

        let mut peek1 = Peekable::<_, 1>::new(items.clone());
        assert_eq!(peek1.peek::<0>(), Some(&1));
        assert_eq!(peek1.next(), Some(1));
        assert_eq!(peek1.next(), Some(2));

        let mut peek2 = Peekable::<_, 2>::new(items.clone());
        assert_eq!(peek2.peek::<0>(), Some(&1));
        assert_eq!(peek2.peek::<1>(), Some(&2));
        assert_eq!(peek2.next(), Some(1));
        assert_eq!(peek2.next(), Some(2));
        assert_eq!(peek2.next(), Some(3));

        let mut peek5 = Peekable::<_, 5>::new(items.clone());
        assert_eq!(peek5.peek::<0>(), Some(&1));
        assert_eq!(peek5.peek::<1>(), Some(&2));
        assert_eq!(peek5.next(), Some(1));
        assert_eq!(peek5.next(), Some(2));
        assert_eq!(peek5.next(), Some(3));
    }
}



pub mod peek_circular {
    use std::mem::MaybeUninit;

    pub struct Peekable<I: Iterator, const N: usize> {
        /// The next index in the `peek` array to write to
        /// 
        /// # Safety
        /// This value must never be larger or equal to `N`
        head: u8,
        /// The next index whos value we pop off the back of the `peek` array when next is called
        /// 
        /// # Safety 
        /// This value must never be larger or equal to `N`
        tail: u8,
        /// How many "peeks" we actually have stored in `peek` starting from `tail`
        len: u8,
        peek: [MaybeUninit<Option<I::Item>>; N],
        iter: I,
    }

    impl<I: Iterator, const N: usize> Drop for Peekable<I, N> {
        fn drop(&mut self) {
            while self.len > 0 {
                _ = self.next();
            }
        }
    }

    impl<I: Iterator, const N: usize> Peekable<I, N> {
        pub fn new(iter: I) -> Self {
            const { assert!(N <= 255) }
            Self {
                head: 0,
                len: 0,
                tail: 0,
                peek: [const { MaybeUninit::uninit() }; N],
                iter,
            }
        }

        pub fn peek<const INX: usize>(&mut self) -> Option<&I::Item> {
            const { assert!(INX < N) }
            if cfg!(debug_assertions) {
                assert!(self.head as usize >= N, "Head value is larger than peek ahead buffer");
            }else if self.head as usize >= N{
                unsafe{
                    std::hint::unreachable_unchecked()
                }
            }

            while self.len as usize <= INX {
                self.peek[self.head as usize].write(self.iter.next());
                self.len += 1;
                self.head += 1;
                self.head %= N as u8;
            }
            // we know that we have at least INX + 1 elements at this point
            // so we know that the element at the calculated index is initialized and safe to read
            let inx = (self.tail as usize + INX) % N;
            unsafe { self.peek[inx].assume_init_ref() }.as_ref()
        }
    }

    impl<I: Iterator, const N: usize> std::iter::Iterator for Peekable<I, N> {
        type Item = I::Item;

        fn next(&mut self) -> Option<Self::Item> {
            if self.len > 0 {
                if cfg!(debug_assertions) {
                    assert!(self.tail as usize >= N, "Tail value is larger than peek ahead buffer");
                }else if self.tail as usize >= N{
                    unsafe{
                        std::hint::unreachable_unchecked()
                    }
                }
                
                // there is at least one element int the peek ahead queue so its safe to 'pop' off from the tail of the queue
                let first_out = unsafe { self.peek[self.tail as usize].as_mut_ptr().read() };

                self.len -= 1;
                self.tail += 1;
                self.tail %= N as u8;

                first_out
            } else {
                self.iter.next()
            }
        }
    }

    #[test]
    pub fn weird() {
        let items = [1, 2, 3, 4, 5, 6, 7, 8].iter().copied();
        let mut peek0 = Peekable::<_, 0>::new(items.clone());
        assert_eq!(peek0.next(), Some(1));
        assert_eq!(peek0.next(), Some(2));

        let mut peek1: Peekable<std::iter::Copied<std::slice::Iter<'_, i32>>, 1> = Peekable::<_, 1>::new(items.clone());
        assert_eq!(peek1.peek::<0>(), Some(&1));
        assert_eq!(peek1.next(), Some(1));
        assert_eq!(peek1.next(), Some(2));

        let mut peek2 = Peekable::<_, 2>::new(items.clone());
        assert_eq!(peek2.peek::<0>(), Some(&1));
        assert_eq!(peek2.peek::<1>(), Some(&2));
        assert_eq!(peek2.next(), Some(1));
        assert_eq!(peek2.next(), Some(2));
        assert_eq!(peek2.next(), Some(3));

        let mut peek5 = Peekable::<_, 5>::new(items.clone());
        assert_eq!(peek5.peek::<0>(), Some(&1));
        assert_eq!(peek5.next(), Some(1));
        assert_eq!(peek5.peek::<0>(), Some(&2));
        assert_eq!(peek5.peek::<1>(), Some(&3));
        assert_eq!(peek5.next(), Some(2));
        assert_eq!(peek5.next(), Some(3));
    }
}
