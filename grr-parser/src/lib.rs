use combine::parser::char::alpha_num;
use combine::parser::combinator::no_partial;
use combine::parser::combinator::FnOpaque;
use combine::parser::repeat::take_until;
use combine::*;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CodeBlock(pub Vec<CodeStatement>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CodeStatement(pub Vec<CodeExpression>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CodeExpression {
    Identifier(String),
    Quoted(String),
    Block(Box<CodeBlock>),
}

pub fn whitespace<Input>() -> impl Parser<Input>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    skip_many(skip_many1(satisfy(|c| c == ' ' || c == '\t')))
}

pub fn identifier<Input>() -> impl Parser<Input, Output = CodeExpression>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    // TODO : Cleaner implementation
    let identifier_chars = satisfy(|ch: char| {
        ch.is_alphanumeric() || ch == '#' || ch == ':' || ch == '/' || ch == '.'
    })
    .expected("letter, digit or in '#:/.'");
    many1(identifier_chars).map(|identifier: String| CodeExpression::Identifier(identifier))
}

pub fn quoted<Input>() -> impl Parser<Input, Output = CodeExpression>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (token('\"'), take_until(satisfy(|c| c == '\"')), token('\"'))
        .map(|(_, string, _)| CodeExpression::Quoted(string))
}

pub fn block<Input>() -> FnOpaque<Input, CodeExpression>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    // Some black magic is happening to avoid the
    // "recursive opaque type"
    opaque!(no_partial((token('{'), statements(), token('}')).map(
        |(_, stmts, _)| CodeExpression::Block(Box::new(CodeBlock(stmts)))
    )))
}

pub fn element<Input>() -> impl Parser<Input, Output = CodeExpression>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice((identifier(), quoted(), block()))
}

pub fn statement<Input>() -> impl Parser<Input, Output = CodeStatement>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (whitespace(), many(element().skip(whitespace()))).map(|(_, elements)| CodeStatement(elements))
}

pub fn statements<Input>() -> impl Parser<Input, Output = Vec<CodeStatement>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    sep_by(statement(), token('\n'))
}

#[cfg(test)]
mod tests {
    use super::*;
    use combine::stream::position;
    use combine::EasyParser;

    #[test]
    fn simple_indentifier() {
        assert_eq!(
            identifier()
                .easy_parse(position::Stream::new("hello"))
                .map(|t| t.0),
            Ok(CodeExpression::Identifier("hello".into()))
        );
    }
    #[test]

    fn simple_quoted() {
        assert_eq!(
            quoted()
                .easy_parse(position::Stream::new("\"Hello world !\""))
                .map(|t| t.0),
            Ok(CodeExpression::Quoted("Hello world !".into()))
        );
    }

    #[test]
    fn simple_element_indentifier() {
        assert_eq!(
            element()
                .easy_parse(position::Stream::new("hello"))
                .map(|t| t.0),
            Ok(CodeExpression::Identifier("hello".into()))
        );
    }
    #[test]

    fn simple_element_quoted() {
        assert_eq!(
            element()
                .easy_parse(position::Stream::new("\"Hello world !\""))
                .map(|t| t.0),
            Ok(CodeExpression::Quoted("Hello world !".into()))
        );
    }

    #[test]

    fn statement_no_space() {
        assert_eq!(
            statement()
                .easy_parse(position::Stream::new("set a \"Hello world !\""))
                .map(|t| t.0),
            Ok(CodeStatement(vec![
                CodeExpression::Identifier("set".into()),
                CodeExpression::Identifier("a".into()),
                CodeExpression::Quoted("Hello world !".into()),
            ]))
        );
    }

    #[test]

    fn statement_with_space() {
        assert_eq!(
            statement()
                .easy_parse(position::Stream::new("   set a \"Hello world !\"   "))
                .map(|t| t.0),
            Ok(CodeStatement(vec![
                CodeExpression::Identifier("set".into()),
                CodeExpression::Identifier("a".into()),
                CodeExpression::Quoted("Hello world !".into()),
            ]))
        );
    }

    #[test]

    fn multiple_statements() {
        assert_eq!(
            statements()
                .easy_parse(position::Stream::new(
                    "set a \"Hello world !\"\nset b \"Die, cruel world\""
                ))
                .map(|t| t.0),
            Ok(vec![
                CodeStatement(vec![
                    CodeExpression::Identifier("set".into()),
                    CodeExpression::Identifier("a".into()),
                    CodeExpression::Quoted("Hello world !".into()),
                ]),
                CodeStatement(vec![
                    CodeExpression::Identifier("set".into()),
                    CodeExpression::Identifier("b".into()),
                    CodeExpression::Quoted("Die, cruel world".into()),
                ])
            ])
        );
    }

    #[test]
    fn simple_block() {
        assert_eq!(
            statements()
                .easy_parse(position::Stream::new("{\n  a b\n  c d\n}"))
                .map(|t| t.0),
            Ok(vec![CodeStatement(vec![CodeExpression::Block(Box::new(
                CodeBlock(vec![
                    CodeStatement(vec![]),
                    CodeStatement(vec![
                        CodeExpression::Identifier("a".into()),
                        CodeExpression::Identifier("b".into()),
                    ]),
                    CodeStatement(vec![
                        CodeExpression::Identifier("c".into()),
                        CodeExpression::Identifier("d".into()),
                    ]),
                    CodeStatement(vec![]),
                ])
            ))])])
        );
    }
    #[test]
    fn embedded_block() {
        assert_eq!(
            statements()
                .easy_parse(position::Stream::new("a { b c } d"))
                .map(|t| t.0),
            Ok(vec![CodeStatement(vec![
                CodeExpression::Identifier("a".into()),
                CodeExpression::Block(Box::new(CodeBlock(vec![CodeStatement(vec![
                    CodeExpression::Identifier("b".into()),
                    CodeExpression::Identifier("c".into()),
                ])]))),
                CodeExpression::Identifier("d".into()),
            ])])
        );
    }
}
