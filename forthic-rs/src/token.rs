#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    String(String),
    Comment(String),
    StartArray,
    EndArray,
    StartModule(String),
    EndModule,
    StartDefinition(String),
    EndDefinition,
    StartMemo(String),
    Word(String),
    EOS,
}
