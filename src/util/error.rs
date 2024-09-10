use crate::lex::Span;

use super::info::{CompilerInfo, Node, NodeId, Source};

#[allow(unused)]
enum ErrorKind {
    Error,
    Warning,
    Info,
}

#[allow(unused)]
pub struct ErrorNode<'a> {
    node: Option<NodeId>,
    span: Span,
    source: Source<'a>,
    kind: ErrorKind,
    msg: String,
}

impl<'a> ErrorNode<'a> {
    pub fn eof(source: Source<'a>, msg: String) -> ErrorNode<'a> {
        ErrorNode {
            node: None,
            span: source.eof(),
            source,
            kind: ErrorKind::Error,
            msg,
        }
    }

    pub fn span(span: Span, source: Source<'a>, msg: String) -> ErrorNode<'a> {
        ErrorNode {
            node: None,
            span,
            source,
            kind: ErrorKind::Error,
            msg,
        }
    }
}

impl NodeId {
    pub fn error<'a>(&self, info: &CompilerInfo<'a>, msg: String) -> ErrorNode<'a> {
        let (span, source) = info.get_node_source(*self);
        ErrorNode {
            node: Some(*self),
            span,
            source,
            kind: ErrorKind::Error,
            msg,
        }
    }
}

impl<T> Node<T> {
    pub fn error<'a>(&self, info: &CompilerInfo<'a>, msg: String) -> ErrorNode<'a> {
        self.1
            .expect("REPLACE THIS WITH SOMETHING BETTER")
            .error(info, msg)
    }
}
const BOLD: &str = "\x1b[1m";
const RED: &str = "\x1b[31m";
const YELLOW: &str = "\x1b[33m";
const BLUE: &str = "\x1b[34m";
// const GREEN: &str = "\x1b[32m";
const RESET: &str = "\x1b[0;22m";

impl<'a> std::fmt::Display for ErrorNode<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ErrorKind::Error => write!(f, "{BOLD}{RED}error{RESET}{RESET}{BOLD}: {}", self.msg)?,
            ErrorKind::Warning => {
                write!(f, "{BOLD}{YELLOW}warning{RESET}{RESET}{BOLD}: {}", self.msg)?
            }
            ErrorKind::Info => write!(f, "{BOLD}{BLUE}info{RESET}{RESET}{BOLD}: {}", self.msg)?,
        }

        let error_range =
            self.span.offset as usize..(self.span.offset as usize + self.span.len as usize);
        let start = self.source.contents[..error_range.start]
            .char_indices()
            .rev()
            .find_map(|c| (c.1 == '\n').then_some(c.0.saturating_add(1)))
            .unwrap_or(0);
        let to_end = (error_range.end < self.source.contents.len())
            .then(|| &self.source.contents[error_range.end..])
            .unwrap_or_default();
        let end = error_range.end
            + to_end
                .chars()
                .position(|c| c == '\n')
                .unwrap_or(to_end.len());
        let expanded_range = start..end;
        let expanded = &self.source.contents
            [expanded_range.start..expanded_range.end.min(self.source.contents.len())];

        let line = self.span.line + 1;
        let space = (((line as usize + expanded.lines().count()) as f32)
            .log10()
            .floor() as u8) as usize
            + 1;

        writeln!(
            f,
            "{BLUE}{BOLD}\n{: >space$}---> {RESET}{}:{}:{}",
            " ",
            self.source.path,
            line,
            self.span.col + 1
        )?;
        writeln!(f, "{BLUE}{BOLD}{: >space$} |", "")?;
        let mut index = expanded_range.start;
        for (i, line_contents) in expanded.split('\n').enumerate() {
            writeln!(
                f,
                "{: >space$} |{RESET} {}",
                line as usize + i,
                &line_contents
            )?;
            write!(f, "{BLUE}{BOLD}{: >space$} | ", "")?;
            for c in line_contents.chars() {
                if error_range.contains(&index) {
                    write!(f, "~")?;
                } else {
                    write!(f, " ")?;
                }
                index += c.len_utf8();
            }
            //nl
            if error_range.contains(&index) || error_range.is_empty() {
                write!(f, "~")?;
            } else {
                write!(f, " ")?;
            }
            index += '\n'.len_utf8();
            writeln!(f)?;
        }
        write!(f, "{RESET}")
        // if self.error.range.is_some() {

        // } else {
        //     writeln!(f, "{} |{RESET} {}", self.span.line, &line_contents)?;

        //     write!(f, "{BLUE}{BOLD}{space} | ")?;
        //     for _ in line_contents.chars() {
        //         write!(f, " ")?;
        //     }
        //     writeln!(f, "~{RESET}")
        // }
    }
}
