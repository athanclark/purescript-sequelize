"use strict";

var Sequelize = require('sequelize');

exports.emptyModelDefinition = {};

exports.unsafeAddModelDef = function unsafeAddModelDef (k,x,acc) {
  var q = {};
  q[k] = x;
  return Object.assign({},acc,q);
};

exports.unsafeAddDefaultValue = function unsafeAddDefaultValue (acc,v) {
  return Object.assign({},acc,{defaultValue: v});
};

exports.sqlSTRING = Sequelize.STRING;
exports.sqlTEXT = Sequelize.TEXT;
exports.sqlBOOLEAN = Sequelize.BOOLEAN;
exports.sqlDATE = Sequelize.DATE;
exports.sqlINTEGER = Sequelize.INTEGER;
exports.sqlFLOAT = Sequelize.FLOAT;
exports.sqlDOUBLE = Sequelize.DOUBLE;
exports.sqlNOW = Sequelize.NOW;

exports.sqlPgINITIALLYIMMEDIATE = Sequelize.Deferrable.INITIALLY_IMMEDIATE;
