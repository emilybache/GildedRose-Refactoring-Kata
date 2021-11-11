"use strict";

module.exports = {
  require: [
    "ts-node/register",
    "tsconfig-paths/register",
    "source-map-support/register"
  ],
  recursive: true,
  spec: "test/mocha/*.spec.ts"
}
