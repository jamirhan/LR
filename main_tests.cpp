#include "LR.h"
#include <gtest/gtest.h>

namespace Samples {
    std::vector<char> basic_non_terminals = {'A', 'B', 'C', 'D', 'S', 'T', 'R', 'U', 'H', 'L'};
    std::vector<char> basic_terminals = {'a', 'b', 'c', 'd', 'e', '(', ')', 'r', 'u', 'h'};
};

LRBasic parser_creator(const LRBasic::BasicGrammarType& grammar, char init) {
    return LRBasic(Samples::basic_terminals, Samples::basic_non_terminals, grammar, init);
}

TEST(FINITE_TESTS, SIMPLE_LANGS) {

    LRBasic::BasicGrammarType simple_grammar1 = {
            {'S', {"AA"}},
            {'A', {"B"}},
            {'B', {"b"}}
    };
    LRBasic parser = parser_creator(simple_grammar1, 'S');
    EXPECT_TRUE(parser.parse("bb"));
    EXPECT_FALSE(parser.parse("ab"));
    EXPECT_FALSE(parser.parse(""));
    EXPECT_FALSE(parser.parse("aaaaa"));
    EXPECT_THROW(parser.parse("Help me if you can, I'm feeling down"), std::invalid_argument);
    EXPECT_THROW(parser.parse("A"), std::invalid_argument);

    LRBasic::BasicGrammarType simple_grammar2 = {
            {'S', {"AB"}},
            {'A', {"a"}},
            {'B', {"b"}}
    };
    parser = parser_creator(simple_grammar2, 'S');
    EXPECT_TRUE(parser.parse("ab"));
    EXPECT_FALSE(parser.parse("aa"));
    EXPECT_FALSE(parser.parse("aaa"));
    EXPECT_FALSE(parser.parse(""));

    LRBasic::BasicGrammarType simple_grammar3 = {
            {'S', {"ABCD"}},
            {'A', {"a", "b"}},
            {'B', {"a", "b"}},
            {'C', {"a", "b"}},
            {'D', {"a", "b"}},
    };
    parser = parser_creator(simple_grammar3, 'S');
    for (int i = 0; i < 16; ++i) {
        std::string s;
        for (int ind = 0; ind < 4; ++ind) {
            if ((i >> ind) & 1) s += "a";
            else s += "b";
        }
        EXPECT_TRUE(parser.parse(s));
    }

    EXPECT_FALSE(parser.parse("ca"));
    EXPECT_FALSE(parser.parse("aab"));
    EXPECT_FALSE(parser.parse(""));

    LRBasic::BasicGrammarType simple_grammar4 = {
            {'S', {"AA"},},
            {'A', {"a", "b", "cd", "eeee"}}
    };
    parser = parser_creator(simple_grammar4, 'S');
    EXPECT_TRUE(parser.parse("ab"));
    EXPECT_TRUE(parser.parse("eeeeeeee"));
    EXPECT_TRUE(parser.parse("eeeecd"));
    EXPECT_FALSE(parser.parse("a"));
    EXPECT_TRUE(parser.parse("acd"));

    LRBasic::BasicGrammarType mixer = {
            {'S', {"AaBaCa"}},
            {'A', {"a"}},
            {'B', {"b"}},
            {'C', {"c"}}
    };
    parser = parser_creator(mixer, 'S');
    EXPECT_TRUE(parser.parse("aabaca"));
    EXPECT_FALSE(parser.parse("aabac"));
    EXPECT_FALSE(parser.parse("abc"));
    EXPECT_FALSE(parser.parse("aaa"));

    LRBasic::BasicGrammarType BRUH = {
            {'S', {"BRUH"}},
            {'B', {"b"}},
            {'R', {"r"}},
            {'U', {"u"}},
            {'H', {"h"}}
    };
    parser = parser_creator(BRUH, 'S');
    EXPECT_TRUE(parser.parse("bruh"));

}

TEST(FINITE_TESTS, ADDING_EPSILON) {
    LRBasic::BasicGrammarType grammar1 = {
            {'S', {"ABCD"}},
            {'A', {"a", ""}},
            {'B', {"b", ""}},
            {'C', {"c", ""}},
            {'D', {"d", ""}}
    };
    LRBasic parser = parser_creator(grammar1, 'S');
    EXPECT_TRUE(parser.parse(""));
    EXPECT_TRUE(parser.parse("abcd"));
    EXPECT_TRUE(parser.parse("acd"));
    EXPECT_TRUE(parser.parse("abd"));
    EXPECT_TRUE(parser.parse("bcd"));
    EXPECT_FALSE(parser.parse("aa"));
    EXPECT_FALSE(parser.parse("bb"));
    EXPECT_TRUE(parser.parse("a"));
    EXPECT_TRUE(parser.parse("b"));
    EXPECT_FALSE(parser.parse("bcda"));
    EXPECT_FALSE(parser.parse("ba"));
    EXPECT_THROW(parser.parse("two-headed boy"), std::invalid_argument);

    LRBasic::BasicGrammarType grammar2 = {
            {'S', {""}}
    };

    parser = parser_creator(grammar2, 'S');

    EXPECT_TRUE(parser.parse(""));
    for (auto el: Samples::basic_terminals) {
        EXPECT_FALSE(parser.parse(std::string(1, el)));
    }

}

TEST(GRAMMAR_CHECKER, GRAMMAR_CHECKER) {
    LRBasic::BasicGrammarType grammar_with_wrong_terminals = {
            {'S', {"AB"}},
            {'A', {"v"}}
    };
    EXPECT_THROW(parser_creator(grammar_with_wrong_terminals, 'S'), LRBasic::BadGrammar);

    LRBasic::BasicGrammarType grammar_without_one_non_terminal = {
            {'S', {"ABC"}},
            {'A', {"a"}},
            {'B', {"b"}}
    };

    EXPECT_THROW(parser_creator(grammar_without_one_non_terminal, 'S'), LRBasic::BadGrammar);

    LRBasic::BasicGrammarType grammar_with_empty_rule = {
            {}
    };
    EXPECT_THROW(parser_creator(grammar_with_empty_rule, 'S'), LRBasic::BadGrammar);

    LRBasic::BasicGrammarType grammar_with_too_many_init = {
            {'S', {"ABC", "AAA"}}
    };
    EXPECT_THROW(parser_creator(grammar_with_too_many_init, 'S'), LRBasic::BadGrammar);

    LRBasic::BasicGrammarType grammar_with_no_init_definition = {
            {'A', {"AA"}}
    };
    EXPECT_THROW(parser_creator(grammar_with_no_init_definition, 'S'), LRBasic::BadGrammar);

    LRBasic::BasicGrammarType grammar_with_dollar = {
            {'A', {"$"}}
    };
    EXPECT_THROW(parser_creator(grammar_with_dollar, 'A'), LRBasic::BadGrammar);

    LRBasic::BasicGrammarType normal_grammar = {
            {'S', {"a"}}
    };

    EXPECT_THROW(LRBasic({'a', 'S'}, {'S'}, normal_grammar, 'S'), LRBasic::BadGrammar);



}

TEST(INFINITE_TESTS, SIMPLE_LANGS) {

    LRBasic::BasicGrammarType infinite_scream_silent { // silent because it doesn't have contain actual words
            {'S', {"A"}},
            {'A', {"Aa"}}
    };

    LRBasic parser = parser_creator(infinite_scream_silent, 'S');
    for (int i = 0; i < 1000; ++i) {
        std::string s = std::string(i, 'a');
        EXPECT_FALSE(parser.parse(std::string(i, 'a')));
    }

    LRBasic::BasicGrammarType infinite_scream_power {
            {'S', {"A"}},
            {'A', {"Aa", ""}}
    };

    parser = parser_creator(infinite_scream_power, 'S');
    for (int i = 0; i < 100; ++i) {
        std::string s = std::string(i, 'a');
        EXPECT_TRUE(parser.parse(std::string(i, 'a')));
    }

    LRBasic::BasicGrammarType HYPER_BRUH = {
            {'S', {"B"}},
            {'B', {"Bbruh", ""}}
    };
    parser = parser_creator(HYPER_BRUH, 'S');

    for (int i = 0; i < 100; ++i) {
        std::string bruh_world;
        for (int j = 0; j < i; ++j) {
            bruh_world += "bruh";
        }
        EXPECT_TRUE(parser.parse(bruh_world));
    }

}

TEST(INFINITE_TESTS, ADVANCES_LANGS) {
    LRBasic::BasicGrammarType BBS = { // Balanced Bracket Sequence
            {'S', {"A"}},
            {'A', {"A(A)", ""}},
    };
    LRBasic parser = parser_creator(BBS, 'S');
    std::vector<std::string> some_cool_bracket_sequences = {
            "(())", "()", "", "((())())()", "()()()()()()()()", "(())((()))(((())))", "(()()(()))(()(()()))", "(()())"
    };

    std::vector<std::string> some_awful_bracket_sequences = {
            "(", ")", "(()", ")(", "))()", ")))))))))", "(()()()))(())()()))()())))", ")))()))()))()()))(()))"
    };

    for (const auto& el: some_cool_bracket_sequences) {
        EXPECT_TRUE(parser.parse(el));
    }

    for (const auto& el: some_awful_bracket_sequences) {
        EXPECT_FALSE(parser.parse(el));
    }

    LRBasic::BasicGrammarType useless_still_advanced = { // a^k c^n d^n b^k
            {'S', {"A"}},
            {'A', {"aAb", "B"}},
            {'B', {"cBd", ""}}
    };

    parser = parser_creator(useless_still_advanced, 'S');

    for (int k = 0; k < 10; ++k) {
        for (int n = 0; n < 10; ++n) {
            std::string s;
            for (int i = 0; i < k; ++i, s += "a");
            for (int i = 0; i < n; ++i, s += "c");
            for (int i = 0; i < n; ++i, s += "d");
            for (int i = 0; i < k; ++i, s += "b");

            EXPECT_TRUE(parser.parse(s));

            s += "a";
            EXPECT_FALSE(parser.parse(s));
        }
    }

}

TEST(INFINITE_TESTS, FULL_POWER) {
    // here we unleash the full power of LR with custom alphabets!

    std::unordered_set<std::string> non_terms = {
            "Init", "BBS", "left_bracket", "right_bracket"
    };

    std::unordered_set<std::string> terms = {
            "(", ")"
    };

    LR::GrammarType cool_BBS = {
            {"Init", {{"BBS"}}},
            {"BBS", {{"BBS", "left_bracket", "BBS", "right_bracket"}, {}}},
            {"left_bracket", {{"("}}},
            {"right_bracket", {{")"}}}
    };

    LR full_power_parser(terms, non_terms, cool_BBS, "Init");

    std::vector<std::vector<std::string>> cool_bbs = {
            {"(", ")"}, {}, {"(", "(", ")", "(", ")", ")"}
    };

    for (const auto& el: cool_bbs) EXPECT_TRUE(full_power_parser.parse(el.begin(), el.end()));

    std::unordered_set<std::string> non_terms_add = {
            "S'", "START", "ADD", "FACTOR", "TERM"
    };

    std::unordered_set<std::string> terms_add = {
            "a", "b", "(", ")", "+"
    };

    LR::GrammarType grammar_add = {
            {"S'", {{"START"}}},
            {"START", {{"ADD"}}},
            {"ADD", {{"ADD", "+", "FACTOR"}, {"FACTOR"}}},
            {"FACTOR", {{"TERM"}}},
            {"TERM", {{"(", "ADD", ")"}, {"a"}, {"b"}}}
    };

    full_power_parser = LR(terms_add, non_terms_add, grammar_add, "S'");

    std::vector<std::vector<std::string>> valid_expressions = {
            {"(", "a", "+", "b", ")"},
            {"(", "a", ")"},
            {"(", "a", "+", "a", "+", "b", "+", "a", ")"}
    };

    for (auto& el: valid_expressions)
        EXPECT_TRUE(full_power_parser.parse(el.begin(), el.end()));

}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
