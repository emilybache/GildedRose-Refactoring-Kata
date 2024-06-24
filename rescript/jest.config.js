module.exports = {
  moduleFileExtensions: [
    "js",
    "mjs",
  ],
  testMatch: [
    "**/src/**/*_test.mjs",
    "**/src/**/*_test.bs.js",
  ],
  transform: {
    "^.+\\.jsx?$": "esbuild-jest"
  },
  "transformIgnorePatterns": ["<rootDir>/node_modules/(?!(rescript|@glennsl/rescript-jest)/)"]
}
