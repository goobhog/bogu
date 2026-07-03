\version "2.24.0"
\header { title = "test" }
\score {
  \new Staff {
    \time 4/4
    c'4 d'4 e'4 f'4 \cadenzaOn \omit Stem r32 g'2 e'2 d'2 a'2 d'2 e'2 c'2 c'2 a'2 a'2 a'2 d'2 \cadenzaOff \undo \omit Stem \bar "|" g'4 c''4 c''2 
  }
  \layout { \context { \Voice \remove "Note_heads_engraver" \consists "Completion_heads_engraver" } }
}
