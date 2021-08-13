/* exports.measureTextImpl = () => {
  const canvas = new OffscreenCanvas(1000, 1000);
  const ctx = canvas.getContext("2d");

  return (font) => (text) => {
    ctx.save();
    ctx.font = font;

    const result = ctx.measureText(text);

    ctx.restore();

    return result;
  };
};
 */

exports.measureText = (ctx) => (font) => (text) => {
  ctx.save();
  ctx.font = font;

  const result = ctx.measureText(text);

  ctx.restore();

  return result;
};
