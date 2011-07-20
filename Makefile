# Makefile

CC=gcc

submission-bot: ants-bot.asd ants-common.asd src/*.lisp src-common/*.lisp
	sbcl --script MyBot.lisp;

local-bot: ants-bot.asd ants-common.asd src/*.lisp src-common/*.lisp
	sbcl --script bin/compile-local-bot.sbcl;

play-game: play-game.asd ants-common.asd src-play-game/*.lisp src-common/*.lisp
	CC=${CC} sbcl --script bin/compile-play-game.sbcl;

proxy-bot: proxy-bot.asd ants-common.asd src-proxy-bot/*.lisp src-common/*.lisp
	CC=${CC} sbcl --script bin/compile-proxy-bot.sbcl;

submission-zip: ants-bot.asd ants-common.asd MyBot.lisp src/*.lisp src-common/*.lisp
	( rm -f common_lisp_submission.zip ; zip -r common_lisp_submission.zip ants-bot.asd ants-common.asd MyBot.lisp src/*.lisp src-common/*.lisp 3rd-party/; )

clean:
	rm -f MyBot ants-bot common_lisp_submission.zip play-game proxy-bot *.log;

play:
	python tools/playgame.py \
	--end_wait=0.25 \
	--verbose \
	--log_dir game_logs \
	--turns 600 \
	--map_file /Users/mechairoi/repos/google-ai-challenge-2011-1-ants/tools/maps/symmetric_maps/symmetric_40.map \
	--log_error \
	"python /Users/mechairoi/repos/google-ai-challenge-2011-1-ants/tools/sample_bots/python/GreedyBot.py" \
	"python /Users/mechairoi/repos/google-ai-challenge-2011-1-ants/tools/sample_bots/python/GreedyBot.py" \
	"python /Users/mechairoi/repos/google-ai-challenge-2011-1-ants/tools/sample_bots/python/GreedyBot.py" \
	"python /Users/mechairoi/repos/google-ai-challenge-2011-1-ants/tools/sample_bots/python/GreedyBot.py" \
	"./MyBot"

#	"-e",
#	"ruby /Users/yohei/working/gai/bot_ruby/MyBot.rb",
