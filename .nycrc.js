module.exports = {
  extension: [
    ".ts"
  ],
  exclude: [
    "**/*.d.ts",
    "test/**",
    "/*.js"
  ],
  require: [
    "ts-node/register"
  ],
  reporter: [
    "html",
    "text"
  ]
}
