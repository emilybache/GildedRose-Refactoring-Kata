test: elm.json package.json elm-test
	elm-test --watch

elm-test:
	which elm-test || npm install elm-test --save-dev
	grep elm-explorations/test elm.json || elm-test init

package.json:
	echo '{}' > package.json

elm.json:
	which elm || (echo "First, install elm https://guide.elm-lang.org/install.html"; exit 1)
	elm init
