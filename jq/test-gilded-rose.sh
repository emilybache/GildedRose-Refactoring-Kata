#!/usr/bin/env bash

jq -en "$(cat gilded-rose.jq)"'[{name:"foo",sell_in:0,quality:0}] | update_quality | .[].name == "fixme"'
