# Plan: Enhance Forthic-TS Core with Word Descriptions

## Overview
This plan outlines how to enhance the core Forthic TypeScript implementation (`~/products/forthic/forthic-ts`) with built-in word description support, making it a first-class feature available to all Forthic applications.

## Core Changes Required

### 1. Enhanced Module Base Class (`src/forthic/module.ts`)

#### Add Metadata Interfaces
```typescript
interface WordMetadata {
  description: string;
  usage: string;
  example?: string;
  category?: string;
  tags?: string[];
  seeAlso?: string[];
}

interface WordRegistry {
  word: Word;
  metadata: WordMetadata;
}
```

#### Extend Module Class
```typescript
export class Module {
  words: Word[];
  exportable: string[];
  variables: { [key: string]: Variable };
  modules: { [key: string]: Module };
  // NEW: Add metadata tracking
  private wordDescriptions: Map<string, WordMetadata> = new Map();
  
  // ENHANCED: Add metadata support to word registration
  add_module_word(word_name: string, word_func: WordHandler, metadata?: WordMetadata): void {
    // Existing implementation
    const word = new ModuleWord(word_name, word_func);
    this.add_word(word);
    
    // NEW: Store metadata if provided
    if (metadata) {
      this.wordDescriptions.set(word_name, metadata);
    }
  }
  
  // NEW: Description discovery methods
  getWordDescription(wordName: string): WordMetadata | undefined {
    return this.wordDescriptions.get(wordName);
  }
  
  getAllDescriptions(): Map<string, WordMetadata> {
    return new Map(this.wordDescriptions);
  }
  
  getWordsByCategory(category: string): string[] {
    const words: string[] = [];
    for (const [name, metadata] of this.wordDescriptions) {
      if (metadata.category === category) {
        words.push(name);
      }
    }
    return words;
  }
  
  searchWords(query: string): string[] {
    const results: string[] = [];
    const queryLower = query.toLowerCase();
    
    for (const [name, metadata] of this.wordDescriptions) {
      if (name.toLowerCase().includes(queryLower) ||
          metadata.description.toLowerCase().includes(queryLower) ||
          metadata.tags?.some(tag => tag.toLowerCase().includes(queryLower))) {
        results.push(name);
      }
    }
    return results;
  }
}
```

### 2. Global Module Enhancement (`src/modules/global.ts`)

Add descriptions for all built-in words:

```typescript
export class GlobalModule extends Module {
  constructor(interp: Interpreter) {
    super("global", interp);
    
    // Stack manipulation
    this.add_module_word("DUP", this.word_DUP.bind(this), {
      description: "Duplicate the top stack item",
      usage: "( a -- a a )",
      example: "5 DUP  // pushes 5 5",
      category: "stack"
    });
    
    this.add_module_word("SWAP", this.word_SWAP.bind(this), {
      description: "Swap the top two stack items", 
      usage: "( a b -- b a )",
      example: "1 2 SWAP  // results in 2 1",
      category: "stack"
    });
    
    this.add_module_word("ROT", this.word_ROT.bind(this), {
      description: "Rotate top three items: third becomes first",
      usage: "( a b c -- b c a )",
      example: "1 2 3 ROT  // results in 2 3 1", 
      category: "stack"
    });
    
    this.add_module_word("DROP", this.word_DROP.bind(this), {
      description: "Remove the top stack item",
      usage: "( a -- )",
      example: "1 2 DROP  // leaves just 1",
      category: "stack"
    });
    
    // Arithmetic  
    this.add_module_word("+", this.word_PLUS.bind(this), {
      description: "Add two numbers",
      usage: "( a b -- sum )",
      example: "3 4 +  // pushes 7",
      category: "arithmetic"
    });
    
    this.add_module_word("-", this.word_MINUS.bind(this), {
      description: "Subtract second number from first",
      usage: "( a b -- difference )",
      example: "10 3 -  // pushes 7",
      category: "arithmetic"
    });
    
    this.add_module_word("*", this.word_TIMES.bind(this), {
      description: "Multiply two numbers",
      usage: "( a b -- product )",
      example: "4 5 *  // pushes 20",
      category: "arithmetic"
    });
    
    this.add_module_word("/", this.word_DIVIDE.bind(this), {
      description: "Divide first number by second",
      usage: "( a b -- quotient )",
      example: "15 3 /  // pushes 5",
      category: "arithmetic"
    });
    
    // String operations
    this.add_module_word("LENGTH", this.word_LENGTH.bind(this), {
      description: "Get length of string or array",
      usage: "( string|array -- length )",
      example: '"hello" LENGTH  // pushes 5',
      category: "string",
      tags: ["string", "array", "size"]
    });
    
    this.add_module_word("CONCAT", this.word_CONCAT.bind(this), {
      description: "Concatenate two strings",
      usage: "( str1 str2 -- concatenated )",
      example: '"hello" " world" CONCAT  // pushes "hello world"',
      category: "string"
    });
    
    // Control flow
    this.add_module_word("IF", this.word_IF.bind(this), {
      description: "Conditional execution based on boolean value",
      usage: "( condition true-branch false-branch -- )",
      example: 'true "yes" "no" IF  // executes "yes"',
      category: "control",
      tags: ["conditional", "branch"]
    });
    
    // Comparison
    this.add_module_word("=", this.word_EQUAL.bind(this), {
      description: "Test if two values are equal",
      usage: "( a b -- boolean )",
      example: "5 5 =  // pushes true",
      category: "comparison"
    });
    
    this.add_module_word("<", this.word_LESS_THAN.bind(this), {
      description: "Test if first value is less than second",
      usage: "( a b -- boolean )",
      example: "3 5 <  // pushes true",
      category: "comparison"
    });
    
    this.add_module_word(">", this.word_GREATER_THAN.bind(this), {
      description: "Test if first value is greater than second", 
      usage: "( a b -- boolean )",
      example: "7 5 >  // pushes true",
      category: "comparison"
    });
  }
}
```

### 3. Interpreter Enhancement (`src/forthic/interpreter.ts`)

Add methods to access descriptions through the interpreter:

```typescript
export class Interpreter {
  // ... existing code ...
  
  // NEW: Description discovery methods
  getWordDescription(wordName: string): WordMetadata | undefined {
    const word = this.find_word(wordName);
    if (!word) return undefined;
    
    // Find which module contains this word and get its description
    for (const [moduleName, module] of this.registered_modules.entries()) {
      const description = module.getWordDescription(wordName);
      if (description) {
        return description;
      }
    }
    return undefined;
  }
  
  getAllWordsWithDescriptions(): Array<{ word: string; module: string; metadata: WordMetadata }> {
    const results: Array<{ word: string; module: string; metadata: WordMetadata }> = [];
    
    for (const [moduleName, module] of this.registered_modules.entries()) {
      const descriptions = module.getAllDescriptions();
      for (const [wordName, metadata] of descriptions) {
        results.push({ word: wordName, module: moduleName, metadata });
      }
    }
    
    return results.sort((a, b) => a.word.localeCompare(b.word));
  }
  
  searchWords(query: string): Array<{ word: string; module: string; metadata: WordMetadata }> {
    const results: Array<{ word: string; module: string; metadata: WordMetadata }> = [];
    
    for (const [moduleName, module] of this.registered_modules.entries()) {
      const matchingWords = module.searchWords(query);
      for (const wordName of matchingWords) {
        const metadata = module.getWordDescription(wordName);
        if (metadata) {
          results.push({ word: wordName, module: moduleName, metadata });
        }
      }
    }
    
    return results;
  }
  
  getWordsByCategory(category: string): Array<{ word: string; module: string; metadata: WordMetadata }> {
    const results: Array<{ word: string; module: string; metadata: WordMetadata }> = [];
    
    for (const [moduleName, module] of this.registered_modules.entries()) {
      const categoryWords = module.getWordsByCategory(category);
      for (const wordName of categoryWords) {
        const metadata = module.getWordDescription(wordName);
        if (metadata) {
          results.push({ word: wordName, module: moduleName, metadata });
        }
      }
    }
    
    return results;
  }
}
```

### 4. Enhanced Introspection Module

Create a comprehensive introspection module in the core:

```typescript
export class IntrospectionModule extends Module {
  constructor(interp: Interpreter) {
    super("introspection", interp);
    
    // Existing words...
    this.add_module_word("WORDS", this.word_words.bind(this), {
      description: "List all available words with modules",
      usage: "( -- word-array )",
      category: "introspection"
    });
    
    // NEW: Description-aware words
    this.add_module_word("WORD-DESCRIPTION", this.word_word_description.bind(this), {
      description: "Get description of a word",
      usage: "( word-name -- description )",
      example: '"DUP" WORD-DESCRIPTION',
      category: "introspection"
    });
    
    this.add_module_word("WORD-USAGE", this.word_word_usage.bind(this), {
      description: "Get usage pattern of a word",
      usage: "( word-name -- usage-string )",
      example: '"DUP" WORD-USAGE',
      category: "introspection"
    });
    
    this.add_module_word("WORD-EXAMPLE", this.word_word_example.bind(this), {
      description: "Get example usage of a word",
      usage: "( word-name -- example-string )",
      example: '"DUP" WORD-EXAMPLE',
      category: "introspection"
    });
    
    this.add_module_word("SEARCH-WORDS", this.word_search_words.bind(this), {
      description: "Search for words by name or description",
      usage: "( query-string -- matching-words-array )",
      example: '"stack" SEARCH-WORDS',
      category: "introspection"
    });
    
    this.add_module_word("WORDS-BY-CATEGORY", this.word_words_by_category.bind(this), {
      description: "Get all words in a category",
      usage: "( category-string -- words-array )",
      example: '"arithmetic" WORDS-BY-CATEGORY',
      category: "introspection"
    });
    
    this.add_module_word("HELP", this.word_help.bind(this), {
      description: "Get comprehensive help for a word",
      usage: "( word-name -- help-info-json )",
      example: '"DUP" HELP',
      category: "introspection"
    });
  }
  
  // Implementation of new description-aware words...
  async word_word_description(interp: Interpreter): Promise<void> {
    const wordName = String(interp.stack_pop());
    const description = interp.getWordDescription(wordName);
    interp.stack_push(description?.description || "No description available");
  }
  
  async word_help(interp: Interpreter): Promise<void> {
    const wordName = String(interp.stack_pop());
    const description = interp.getWordDescription(wordName);
    
    if (description) {
      const help = {
        name: wordName,
        description: description.description,
        usage: description.usage,
        example: description.example,
        category: description.category,
        tags: description.tags,
        seeAlso: description.seeAlso
      };
      interp.stack_push(JSON.stringify(help, null, 2));
    } else {
      interp.stack_push(`No help available for word: ${wordName}`);
    }
  }
}
```

## Implementation Timeline

### Phase 1: Core Infrastructure (Week 1)
1. **Update module.ts**: Add WordMetadata interface and description methods
2. **Update interpreter.ts**: Add description discovery methods  
3. **Create tests**: Unit tests for description functionality
4. **Update TypeScript definitions**: Export new interfaces

### Phase 2: Standard Library Descriptions (Week 2)
1. **GlobalModule**: Add descriptions for all built-in words (~50+ words)
2. **IntrospectionModule**: Enhance with description-aware words
3. **Other core modules**: Add descriptions to any other standard modules
4. **Validation**: Ensure all words have proper metadata

### Phase 3: Advanced Features (Week 3)  
1. **Search functionality**: Implement word search by description
2. **Category system**: Organize words into logical categories
3. **Help system**: Comprehensive help command
4. **Documentation generation**: Auto-generate docs from descriptions

### Phase 4: Integration & Testing (Week 4)
1. **Integration tests**: Test description system across modules
2. **Performance testing**: Ensure no significant overhead
3. **Documentation**: Update forthic-ts documentation
4. **Version bump**: Prepare release with description support

## Benefits for Ecosystem

### For Application Developers:
- Rich autocomplete in IDEs and REPLs
- Built-in help system for all words
- Better debugging and introspection
- Searchable word library

### For CLI Applications:
- Professional help systems
- Context-sensitive documentation
- Enhanced user experience
- Better discoverability

### For Language Evolution:
- Self-documenting modules
- Consistent metadata across ecosystem
- Foundation for advanced tooling
- Better learning experience

## Migration Path

### Backward Compatibility:
- All existing code continues to work unchanged
- Metadata is optional during transition
- Gradual adoption across modules

### Gradual Enhancement:
1. Core system with optional metadata
2. Standard library descriptions 
3. Third-party module adoption
4. Tooling integration

This enhancement makes Forthic a more professional and discoverable language while maintaining its simplicity and power.