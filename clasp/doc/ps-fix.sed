/format-rotation/d
/format-x-translation/d
/format-y-translation/d
/^.new-matrix/d
/^new-matrix/d
/format-rotation rotate/d
/%%EndProlog/{
 i\
 /in {72 mul} def \
 /format-rotation 90 def \
 /format-x-translation 1 in def \
 /format-y-translation -12.5 in def \
 /new-matrix {format-rotation rotate format-x-translation format-y-translation translate} def \
 /new-page {showpage new-matrix} def \
 %%EndProlog \
 new-matrix
 d
}
