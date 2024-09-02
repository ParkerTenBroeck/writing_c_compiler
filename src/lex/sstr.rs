use std::marker::PhantomData;

#[repr(packed(4))]
#[derive(Clone, Copy)]
pub struct Sstr<'a> {
    size: u32,
    ptr: *const u8,
    _phantom2: PhantomData<&'a str>,
}

impl<'a> Sstr<'a> {
    pub fn from_str(str: &'a str) -> Self {
        Self {
            ptr: str.as_ptr(),
            size: str.len() as u32,
            _phantom2: Default::default(),
        }
    }

    pub fn as_str(&self) -> &'a str {
        unsafe {
            let slice = std::slice::from_raw_parts(self.ptr, self.size as usize);
            std::str::from_utf8_unchecked(slice)
        }
    }
}

impl<'a> std::ops::Deref for Sstr<'a> {
    type Target = str;

    fn deref(&self) -> &'a Self::Target {
        self.as_str()
    }
}

impl<'a> From<&'a str> for Sstr<'a> {
    fn from(value: &'a str) -> Self {
        Self::from_str(value)
    }
}

impl<'a> From<Sstr<'a>> for &'a str {
    fn from(value: Sstr<'a>) -> Self {
        value.as_str()
    }
}

impl<'a> From<&Sstr<'a>> for &'a str {
    fn from(value: &Sstr<'a>) -> Self {
        value.as_str()
    }
}

impl<'a> From<Sstr<'a>> for String {
    fn from(value: Sstr<'a>) -> Self {
        value.as_str().into()
    }
}

impl<'a> From<&Sstr<'a>> for String {
    fn from(value: &Sstr<'a>) -> Self {
        value.as_str().into()
    }
}

impl<'a> std::fmt::Debug for Sstr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.as_str(), f)
    }
}

impl<'a> std::fmt::Display for Sstr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}

impl<'a> std::cmp::PartialEq for Sstr<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl<'a> std::cmp::PartialEq<str> for Sstr<'a> {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl<'a, 'b> std::cmp::PartialEq<&'b str> for Sstr<'a> {
    fn eq(&self, other: &&'b str) -> bool {
        self.as_str() == *other
    }
}

impl<'a> std::cmp::PartialEq<Sstr<'a>> for str {
    fn eq(&self, other: &Sstr<'a>) -> bool {
        self == other.as_str()
    }
}

impl<'a, 'b> std::cmp::PartialEq<Sstr<'a>> for &'b str {
    fn eq(&self, other: &Sstr<'a>) -> bool {
        *self == other.as_str()
    }
}

impl<'a> std::cmp::Eq for Sstr<'a> {}

impl<'a> std::hash::Hash for Sstr<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}
