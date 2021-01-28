"use strict";

module.exports = {
  require: [
    "ts-node/register",
    "source-map-support/register"
  ],
  recursive: true,
  spec: "test/*.spec.ts"
}