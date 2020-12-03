function getQualityChange(item) {
  return 0;
}

function is(item) {
  return item.name.toLowerCase().match(/sulfuras/);
};

module.exports = { is, getQualityChange };