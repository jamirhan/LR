#include <string>
#include <utility>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <exception>
#include <functional>
#include <queue>
#include <stack>
#include <stdexcept>


class LR {
public:
    using SymbolType = std::string;
    using GrammarType = std::unordered_map<SymbolType, std::vector<std::vector<SymbolType>>>;
protected:
    std::unordered_set<SymbolType> terminals;
    std::unordered_set<SymbolType> non_terminals;
    GrammarType grammar;
    SymbolType initial_symbol;

    struct State {
        SymbolType non_term;
        size_t rule_num;
        size_t point_before;
        SymbolType following_term;
        State() = delete;
        State(SymbolType non_term, size_t rule_num, size_t point_before, SymbolType following_term):
        non_term(std::move(non_term)), rule_num(rule_num),
        point_before(point_before), following_term(std::move(following_term)) { };

        bool operator==(const State& another) const {
            return non_term == another.non_term && rule_num == another.rule_num &&
            point_before == another.point_before && following_term == another.following_term;
        }

        // does not check anything
        void advance_point() {
            ++point_before;
        }

        [[nodiscard]] SymbolType stays_before(const LR& parent_class) const {
            if (point_before == parent_class.grammar.at(non_term)[rule_num].size()) {
                return "";
            }
            return parent_class.grammar.at(non_term)[rule_num][point_before];
        }

    };

    struct StateHash {
        const static size_t p = 13;
        const static size_t q = 19;
        const static size_t r = 31;
        const static size_t m = 1e9 + 7;
        size_t operator()(const State& state) const {
            size_t first_item = (p * state.rule_num) % m;
            size_t second_item = (q * state.point_before) % m;
            size_t combine = (p*(first_item + second_item)) % m;
            size_t str_hash = (std::hash<SymbolType>()(state.following_term) * r) % m;
            size_t non_term_hash = std::hash<SymbolType>()(state.non_term);
            return (combine + str_hash + non_term_hash) % m;
        }
    };

    struct Node {
        std::unordered_set<State, StateHash> closure_states;
        std::unordered_map<SymbolType, size_t> goto_table;

        bool operator==(const Node& another) const {
            return closure_states == another.closure_states;
        }

        void node_complete() {

        }

        State wrap(std::stack<SymbolType> cur_word, const SymbolType& next_symb, const LR& parent_class) {
            std::unordered_set<State, StateHash> cur_candidates;
            for (auto& state: closure_states) {
                if (state.stays_before(parent_class).empty() && state.following_term == next_symb)
                    cur_candidates.insert(state);
            }
            std::function<void(std::unordered_set<State, StateHash>&)> complete =
                    [&cur_word, &parent_class](std::unordered_set<State, StateHash>& cur) {
                std::unordered_set<State, StateHash> result;
                for (auto candidate: cur) {
                    if (candidate.point_before != 0) {
                        if (cur_word.empty()) continue;
                        --candidate.point_before;
                        if (candidate.stays_before(parent_class) == cur_word.top()) {
                            result.emplace(std::move(candidate));
                        }
                    } else {
                        result.emplace(std::move(candidate));
                    }
                }
                if (!cur_word.empty())
                    cur_word.pop();
                cur = result;
            };
            cur_candidates = LR::transitive_closure(cur_candidates, complete);
            if (cur_candidates.empty()) throw std::invalid_argument("");
            if (cur_candidates.size() > 1) throw BadGrammar("invalid state exists");
            return *cur_candidates.begin();
        }

    };

    struct NodeHash {
        size_t operator()(const Node& nd) const {
            size_t ans = 0;
            StateHash hasher;
            for (const auto& el: nd.closure_states) {
                ans += hasher(el);
            }
            return ans;
        }
    };

    std::unordered_map<SymbolType , std::vector<SymbolType>> First_table;
    std::unordered_set<SymbolType> epsilon_generating;
    std::vector<Node> nodes;

    template <typename T>
    static T transitive_closure(T initial_state, const std::function<void(T&)>& f) {
        T last_state = initial_state;
        T cur_state = initial_state;
        f(cur_state);
        while (last_state != cur_state) {
            last_state = cur_state;
            f(cur_state);
        }
        return cur_state;
    }

    void build_first_table() {
        struct DecayedCell {
            std::unordered_set<SymbolType> terminals;
            std::unordered_set<SymbolType> non_terminals;
            bool operator==(const DecayedCell& another) const {
                return terminals == another.terminals && non_terminals == another.non_terminals;
            }
        };

        using Table = std::unordered_map<SymbolType, DecayedCell>;
        Table decayed_table;
        for (const auto& el: grammar) {
            for (const auto& right_rule: el.second) {
                if (right_rule.empty()) {
                    decayed_table[el.first].terminals.insert("");
                } else if (terminals.contains(right_rule[0])) {
                    decayed_table[el.first].terminals.insert(right_rule[0]);
                } else if (right_rule[0] != el.first) {
                    decayed_table[el.first].non_terminals.insert(right_rule[0]);
                }
            }
        }



        std::function<void(Table&)> complete = [this](Table& cur_table) {
            for (auto& el: cur_table) {
                for (const auto& rule: grammar[el.first]) {
                    bool cont = true;
                    for (size_t ind = 0; ind < rule.size() && cont; ++ind) {
                        cont = false;
                        if (terminals.contains(rule[ind])) {
                            cur_table[el.first].terminals.insert(rule[ind]);
                        } else {
                            cur_table[el.first].non_terminals.insert(rule[ind]);
                            if (cur_table[rule[ind]].terminals.contains("")) {
                                cont = true;
                            }
                        }
                    }
                    if (cont) {
                        cur_table[el.first].terminals.insert("");
                    }
                }

                for (const auto& non_term: el.second.non_terminals) {
                    for (const auto& sub_term: cur_table[non_term].terminals) {
                        el.second.terminals.insert(sub_term);
                    }
                    for (const auto& sub_non_term: cur_table[non_term].non_terminals) {
                        el.second.non_terminals.insert(sub_non_term);
                    }
                }
            }
        };

        decayed_table = transitive_closure(decayed_table, complete);
        for (const auto& el: decayed_table) {
            for (const auto& term: el.second.terminals) {
                if (term.empty()) {
                    epsilon_generating.insert(el.first);
                }
                First_table[el.first].push_back(term);
            }
        }
    }


    std::unordered_set<State, StateHash> state_closure(const std::vector<State>& kernel) const {
        using StateSet = std::unordered_set<State, StateHash>;
        StateSet states;
        for (const auto& el: kernel) states.insert(el);

        std::function<void(StateSet&)> complete = [this](StateSet& set_to_change){
            SymbolType cur_non_terminal;
            StateSet cur_set = set_to_change;
            for (const auto& state: set_to_change) {

                if (state.stays_before(*this).empty()) continue;

                const auto& cur_rule = grammar.at(state.non_term)[state.rule_num];
                if (non_terminals.contains(cur_rule[state.point_before])) {
                    cur_non_terminal = cur_rule[state.point_before];
                    if (cur_rule.size() == state.point_before + 1) {
                        for (size_t i = 0; i < grammar.at(cur_non_terminal).size(); ++i) {
                            cur_set.emplace(cur_non_terminal, i, 0, state.following_term);
                        }
                    } else if (non_terminals.contains(cur_rule[state.point_before + 1])) {
                        for (size_t i = 0; i < grammar.at(cur_non_terminal).size(); ++i) {
                            size_t cur_point_ind = state.point_before + 1;
                            bool move_on = true;
                            while (move_on && cur_point_ind < cur_rule.size()) {
                                move_on = false;
                                for (const auto &symb: First_table.at(cur_rule[cur_point_ind])) {
                                    if (!symb.empty()) {
                                        cur_set.emplace(cur_non_terminal, i, 0, symb);
                                    } else {
                                        move_on = true;
                                    }
                                }
                                ++cur_point_ind;
                            }
                            if (move_on) {
                                cur_set.emplace(cur_non_terminal, i, 0, state.following_term);
                            }
                        }
                    } else {
                        for (size_t i = 0; i < grammar.at(cur_non_terminal).size(); ++i) {
                            cur_set.emplace(cur_non_terminal, i, 0,
                                            cur_rule[state.point_before + 1]);
                        }
                    }
                }
            }
            set_to_change = std::move(cur_set);
        };

        auto closure = transitive_closure(states, complete);
        return closure;
    }

    void build_automata() {
        std::queue<size_t> to_process;
        std::unordered_set<State, StateHash> initial_state =
                state_closure({State(initial_symbol, 0, 0, "$")});

        std::unordered_map<Node, size_t, NodeHash> cur_nodes;
        to_process.push(cur_nodes.size());
        Node init_node{initial_state, {}};
        cur_nodes.emplace(std::make_pair(init_node, cur_nodes.size()));
        nodes.push_back(init_node);

        while (!to_process.empty()) {
            size_t cur_node_ind = to_process.front();
            Node cur_node = nodes[cur_node_ind];
            to_process.pop();
            std::unordered_map<SymbolType, std::vector<State>> ordered_node;
            Node new_node;
            for (const auto& el: cur_node.closure_states) {
                const SymbolType& symb = el.stays_before(*this);
                if (symb.empty()) continue;
                ordered_node[symb].push_back(el);
            }

            for (const auto& el: ordered_node) {
                const SymbolType& before = el.first;
                std::vector<State> new_states = el.second;
                for (auto& st: new_states) st.advance_point();
                new_node.closure_states = state_closure(new_states);
                auto p = cur_nodes.insert({new_node, cur_nodes.size()});
                cur_node.goto_table[before] = p.first->second;
                if (p.second) {
                    to_process.push(nodes.size());
                    nodes.push_back(new_node);
                }
            }
            nodes[cur_node_ind] = cur_node;
        }
        for (auto& node: nodes) node.node_complete();
    }

    void check_grammar() {

        if (grammar[initial_symbol].size() != 1) {
            throw BadGrammar("initial_symbol must be included exactly in one rule's left hand side");
        }

        for (const auto& el: grammar) {

            if (el.first.empty()) {
                throw BadGrammar("incorrect rule: left hand side of any rule must not be empty");
            }

            if (!non_terminals.contains(el.first)) {
                throw BadGrammar("incorrect rule: " + std::string(el.first) +
                                            " is not marked as non-terminal");
            }

            if (terminals.contains(el.first)) {
                throw BadGrammar("incorrect alphabets: " + std::string(el.first) +
                                            " are marked both as terminal and non-terminal");
            }

            for (const auto& right_rule: el.second) {

                for (const auto &symb: right_rule) {
                    if (!non_terminals.contains(symb) && !terminals.contains(symb)) {
                        throw BadGrammar("incorrect rule: right hand side of any rule must consist"
                                                    " of either terminal or non-terminal symbols");
                    }
                    if (non_terminals.contains(symb) && !grammar.contains(symb)) {
                        throw BadGrammar("incorrect grammar: every non-terminal must be uncoverable,"
                                         " unlike " + symb);
                    }
                }

            }
        }

        for (const auto& el: terminals) {
            if (non_terminals.contains(el)) {
                throw BadGrammar("incorrect alphabets: " + std::string(el) +
                " is marked both as terminal and non-terminal");
            }
            if (el == "$" || el.empty()) {
                throw BadGrammar("incorrect alphabet: alphabet"
                                            " must not contain neither $, nor empty string");
            }
        }
        for (const auto& el: non_terminals) {
            if (el == "$" || el.empty()) {
                throw BadGrammar("incorrect alphabet: alphabet"
                                            " must contain neither $, nor empty string");
            }
        }
    }

    LR() = default;

public:


    struct BadGrammar: std::logic_error {
        explicit BadGrammar(const std::string& msg): std::logic_error(msg) { };
    };

    [[maybe_unused]] explicit LR(std::unordered_set<SymbolType> terminals, std::unordered_set<SymbolType> non_terminals,
                GrammarType grammar, SymbolType initial_symbol): initial_symbol(std::move(initial_symbol)),
                grammar(std::move(grammar)), terminals(std::move(terminals)), non_terminals(std::move(non_terminals)) {
        check_grammar();
        build_first_table();
        build_automata();
    }

    template <typename ForwardIterator>
    bool parse(ForwardIterator begin, ForwardIterator end) {
        std::vector<SymbolType> word;
        for (; begin != end; ++begin) {
            word.push_back(*begin);
        }
        word.emplace_back("$");
        std::stack<SymbolType> next_symbol;

        for (auto i = (long long) word.size() - 1; i >= 0; --i) {
            next_symbol.emplace(word[i]);
            if (word[i] == "$") continue;
            if (!terminals.contains(word[i])) {
                throw std::invalid_argument("the word must contain only terminals");
            }
        }

        std::stack<SymbolType> read_symbols;
        std::stack<size_t> node_path;
        node_path.push(0);

        while (!(read_symbols.size() == 1 && read_symbols.top() == initial_symbol)) {
            if (!nodes[node_path.top()].goto_table.contains(next_symbol.top())) {
                size_t tries = 0;
                try {
                    while (!nodes[node_path.top()].goto_table.contains(next_symbol.top())) {
                        State state_to_wrap = nodes[node_path.top()].wrap(read_symbols, next_symbol.top(), *this);
                        ++tries;
                        for (size_t i = 0; i < grammar[state_to_wrap.non_term][state_to_wrap.rule_num].size(); ++i) {
                            node_path.pop();
                            read_symbols.pop();
                        }
                        read_symbols.push(state_to_wrap.non_term);
                        if (!nodes[node_path.top()].goto_table.contains(read_symbols.top())) break;
                        node_path.push(nodes[node_path.top()].goto_table[read_symbols.top()]);
                    }
                } catch (const std::invalid_argument&) {
                    if (!tries)
                        return false;
                }
            } else {
                read_symbols.push(next_symbol.top());
                next_symbol.pop();
                node_path.push(nodes[node_path.top()].goto_table[read_symbols.top()]);
            }
        }
        return true;
    }

};


// adapter for better user-experience
struct LRBasic: public LR {
    using BasicGrammarType = std::vector<std::pair<char, std::vector<std::string>>>;
    LRBasic(const std::vector<char>& terminals, const std::vector<char>& non_terminals,
            const BasicGrammarType& grammar ,char initial_symbol): LR() {
        for (auto el: terminals) this->terminals.insert(std::string(1, el));
        for (auto el: non_terminals) this->non_terminals.insert(std::string(1, el));
        this->initial_symbol = std::string(1, initial_symbol);
        for (const auto& p: grammar) {
            std::string non_term = std::string(1, p.first);
            for (const std::string& rule: p.second) {
                std::vector<std::string> rule_wrap;
                for (auto& el: rule) rule_wrap.emplace_back(1, el);
                this->grammar[non_term].push_back(rule_wrap);
            }
        }
        check_grammar();
        build_first_table();
        build_automata();
    }

    bool parse(const std::string& word) {
        std::vector<std::string> str_wrapped;
        for (auto c: word) str_wrapped.emplace_back(1, c);
        return LR::parse(str_wrapped.begin(), str_wrapped.end());
    }
};
