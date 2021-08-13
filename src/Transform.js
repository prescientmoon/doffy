const matrices = require("@thi.ng/matrices");

exports.translate = (amount) => matrices.translation23([], amount);
exports.rotate = (angle) => matrices.rotation23([], angle);
exports.scale = (amount) => matrices.scale23([], amount);
exports.transform = (t, r, s) => matrices.transform23([], t, r, s);
exports.inverse = (matrix) => matrices.invert23([], matrix);
exports.multiplyVector = (matrix) => (vector) =>
  matrices.mulV23([], matrix, vector);
exports.composeMatrices = (a) => (b) => matrices.mulM23([], b, a);
exports.identityMatrix = matrices.identity23([]);
exports.rotateAround = (point) => (angle) => {
  const translation = exports.translate(point.map((x) => -x));
  const rotation = exports.rotate(angle);

  const translateThenRotate = exports.composeMatrices(translation)(rotation);

  return exports.composeMatrices(translateThenRotate)(exports.translate(point));
};

exports.scaleXY = exports.scale;
