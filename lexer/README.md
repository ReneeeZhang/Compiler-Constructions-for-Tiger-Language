# Lexer Documentation
Anshu Dwibhashi, Yuanrui Zhang, Clayton Shafer  
ECE 553 Spring 2021

## Strings
In order to properly handle string literals, our lexer has an additional state `STR`. The lexer enters `STR` when it encounters a double quote, and transitions back to `INITIAL` when it encounters a second double quote. When entering `STR`, a global variable used to detect open strings at EOF is incremented. 
The lexer contains a string ref which is used to keep track of the current string as more characters are added to it. Each time the lexer scans a character, that character is appended to the string. When a double quote is encountered, closing the string, the string token's field is set equal to the ref. 
In addition, the lexer must handle escape sequences inside string literals. This was accomplished using regular expressions. When the lexer encounters a backslash followed by any valid escape character, it continues. A backslash followed by an illegal escape character triggers an error. Originally, our implementation used a third state, `ESC`, which was entered upon a backslash character. However, this made it difficult to append escaped characters to the string ref, as escapes were taken literally (i.e. backslash n instead of a newline). Using regexes instead of a third state fixes that issue. 

## Comments
Our lexer sets up a `COMMENT` state to specially address comment-related lexical analysis. Once `INITIAL` state encounters a _/*_, it transitions to `COMMENT`. In order to deal with nested comments, we set up a global variable called *comment_depth*, initialized with 0. Therefore, every time a _/*_ appears in `COMMENT` state, *comment_depth* increments; Conversely, it decrements when a _*/_ occurs. And if *comment_depth* returns to 0, then we know that we reach the end of comment section, therefore, it should transition back to `INITIAL` state. Moreover, we treat new lines within `COMMENT` the same as those in regular code. As for other symbols, we skip extra handlings and simply call lexical analyzer. 

## End of File
When the EOF token is read, our lexer performs two checks to determine whether or not certain lexical errors have 
occured. For comments and strings, the lexer keeps track of depth using refs, as described above. If either of these 
refs have a value not equal to zero when the file ends, the file contains one or more open strings/comments. In this
case, an error message will be printed.

## Identifiers, Integer Literals and Other Tokens
To identify identifiers, we look for any string that starts with a letter and is composed of letters, digits and underscores. All keyword rules appear before the rule to identify identifiers so that a string that is also the name of a keyword is identified as a keyword rather than an identifier. For integer literals, we look for one or more decimal integer digits and convert the string corresponding to a combination of decimal digits into an integer value. For all other keywords and punctuation-tokens, we match the literal keyword or punctuation and create a token for those matches.

## Errors
### Within `INITIAL` State
If the incoming symbols do not match any reserved keywords nor punctuations, new lines, white spaces and tabs, then we treat it as a error.

### Within `STR` State
Similarly, an error will be raised if the incoming symbols match none of patterns as [Strings](https://gitlab.oit.duke.edu/cms156/ece553project/-/edit/test/lexer/README.md#strings) displays.
