#[cfg(test)]
mod tests {
    use forthic_rs::token::Token;
    use forthic_rs::tokenizer::Tokenizer;

    #[test]
    fn test_basic() {
        let tokenizer = Tokenizer::new("[ ] : DEFINITION ; { } '' WORD  @: MEMO");
        let tokens: Vec<Token> = tokenizer.filter_map(|t| t.ok()).collect();
        assert_eq!(
            tokens,
            vec![
                Token::StartArray,
                Token::EndArray,
                Token::StartDefinition("DEFINITION".to_string()),
                Token::EndDefinition,
                Token::StartModule("".to_string()),
                Token::EndModule,
                Token::String("".to_string()),
                Token::Word("WORD".to_string()),
                Token::StartMemo("MEMO".to_string()),
            ]
        );
    }

    #[test]
    fn test_end_definition() {
        let tokenizer = Tokenizer::new("WORD; WORD2");
        let tokens: Vec<Token> = tokenizer.filter_map(|t| t.ok()).collect();
        assert_eq!(
            tokens,
            vec![
                Token::Word("WORD".to_string()),
                Token::EndDefinition,
                Token::Word("WORD2".to_string()),
            ]
        );
    }

    #[test]
    fn test_start_module() {
        let tokenizer = Tokenizer::new("{ {my-mod");
        let tokens: Vec<Token> = tokenizer.filter_map(|t| t.ok()).collect();
        assert_eq!(
            tokens,
            vec![
                Token::StartModule("".to_string()),
                Token::StartModule("my-mod".to_string()),
            ]
        );
    }

    #[test]
    fn test_strings() {
        let tokenizer = Tokenizer::new(
            "'Single' ^Caret^ '''Triple Single''' ^^^Triple Caret^^^ \x16Single DLE\x16",
        );
        let tokens: Vec<Token> = tokenizer.filter_map(|t| t.ok()).collect();
        assert_eq!(
            tokens,
            vec![
                Token::String("Single".to_string()),
                Token::String("Caret".to_string()),
                Token::String("Triple Single".to_string()),
                Token::String("Triple Caret".to_string()),
                Token::String("Single DLE".to_string()),
            ]
        );
    }

    #[test]
    fn test_arrays() {
        let tokenizer = Tokenizer::new("[1 2] [3[4]]");
        let tokens: Vec<Token> = tokenizer.filter_map(|t| t.ok()).collect();
        assert_eq!(
            tokens,
            vec![
                Token::StartArray,
                Token::Word("1".to_string()),
                Token::Word("2".to_string()),
                Token::EndArray,
                Token::StartArray,
                Token::Word("3".to_string()),
                Token::StartArray,
                Token::Word("4".to_string()),
                Token::EndArray,
                Token::EndArray,
            ]
        );
    }

    #[test]
    fn test_end_module() {
        let tokenizer = Tokenizer::new("WORD1}WORD2");
        let tokens: Vec<Token> = tokenizer.filter_map(|t| t.ok()).collect();
        assert_eq!(
            tokens,
            vec![
                Token::Word("WORD1".to_string()),
                Token::EndModule,
                Token::Word("WORD2".to_string()),
            ]
        );
    }
}
