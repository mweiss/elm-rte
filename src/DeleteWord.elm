module DeleteWord exposing (..)

{-
   This is a helper module taken from the DraftJS logic for determining how to find delete a word.
-}

import Regex



-- TODO: add the following characters to punctuation regex
-- ++ "\u30FB\u3001\u3002\u3008-\u3011\u3014-\u301F\uFF1A-\uFF1F\uFF01-\uFF0F" ++ "\uFF3B-\uFF40\uFF5B-\uFF65\u2E2E\u061F\u066A-\u066C\u061B\u060C\u060D" ++ "\uFD3E\uFD3F\u1801\u0964\u104A\u104B\u2010-\u2027\u2030-\u205E" ++ "\xA1-\xB1\xB4-\xB8\xBA\xBB\xBF"


punctuationRegexString : String
punctuationRegexString =
    "[.,+*?$|#{}()'\\^\\-\\[\\]\\\\\\/!@%\"~=<>_:;]"



-- TODO: add the following UTF8 to chameleon characters regex
-- ++ \u2018\u2019


chameleonCharactersRegexString : String
chameleonCharactersRegexString =
    "[']"


whitespaceAndPunctuationRegexString : String
whitespaceAndPunctuationRegexString =
    "\\s|(?![_])" ++ punctuationRegexString


deleteWordRegexString : String
deleteWordRegexString =
    "^" ++ "(?:" ++ whitespaceAndPunctuationRegexString ++ ")*" ++ "(?:" ++ chameleonCharactersRegexString ++ "|(?!" ++ whitespaceAndPunctuationRegexString ++ ").)*" ++ "(?:(?!" ++ whitespaceAndPunctuationRegexString ++ ").)"


backspaceWordRegexString : String
backspaceWordRegexString =
    "(?:(?!" ++ whitespaceAndPunctuationRegexString ++ ").)" ++ "(?:" ++ chameleonCharactersRegexString ++ "|(?!" ++ whitespaceAndPunctuationRegexString ++ ").)*" ++ "(?:" ++ whitespaceAndPunctuationRegexString ++ ")*" ++ "$"


deleteWordRegex : Regex.Regex
deleteWordRegex =
    Maybe.withDefault Regex.never (Regex.fromString deleteWordRegexString)


backspaceWordRegex : Regex.Regex
backspaceWordRegex =
    Maybe.withDefault Regex.never (Regex.fromString backspaceWordRegexString)
