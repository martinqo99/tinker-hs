all: extract_scores parse_calc_scores stats

extract_scores: extract_scores.hs
	ghc -O2 -fvia-C -optc-O3 $< --make -fforce-recomp

parse_calc_scores: parse_calc_scores.hs
	ghc -O2 -fvia-C -optc-O3 $< --make -fforce-recomp

stats: stats.hs
	.ghc -O2 -fvia-C -optc-O3 $< --make -fforce-recomp
