import * as Tk from './output/Main/index.js';

export function exoLang(args) {
  return new TimekNot(args);
}

export function TimekNot(args) {
  if (args==null) args = {};
  this.timeknot = Tk.launch(args)();
}

TimekNot.prototype.launchDirt = function(args) {
  return Tk.launchDirt();
}

TimekNot.prototype.define = function(args) {
  return Tk.define(this.timeknot)(args)();
}

// TimekNot.prototype.clear = function(args) {
//   return Tk.clear(this.timeknot)(args)();
// }

TimekNot.prototype.render = function(args) {
  return Tk.render(this.timeknot)(args)();
}

TimekNot.prototype.renderStandalone = function(args) {
  return Tk.renderStandalone(this.timeknot)(args)();
}

TimekNot.prototype.setTempo = function(foreignTempo) {
  return Tk.setTempo(this.timeknot)(foreignTempo)();
}