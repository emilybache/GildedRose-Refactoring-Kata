function isBackstagePass(item) {
  return item.name.toLowerCase().match(/backstage pass/);
};

module.exports = { isBackstagePass }