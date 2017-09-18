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
exports.sqlCHAR = Sequelize.CHAR;
exports.sqlTEXT = Sequelize.TEXT;
exports.sqlBOOLEAN = Sequelize.BOOLEAN;
exports.sqlBLOB = Sequelize.BLOB;
exports.sqlDATE = Sequelize.DATE;
exports.sqlINTEGER = Sequelize.INTEGER;
exports.sqlBIGINT = Sequelize.BIGINT;
exports.sqlFLOAT = Sequelize.FLOAT;
exports.sqlDOUBLE = Sequelize.DOUBLE;
exports.sqlDECIMAL = Sequelize.DECIMAL;
exports.sqlREAL = Sequelize.REAL;
exports.sqlUUID = Sequelize.UUID;
exports.sqlJSON = Sequelize.JSON;

exports.sqlNOW = Sequelize.NOW;
exports.sqlUUIDV1 = Sequelize.UUIDV1;
exports.sqlUUIDV4 = Sequelize.UUIDV4;
exports.sqlPgINITIALLYIMMEDIATE = Sequelize.Deferrable.INITIALLY_IMMEDIATE;
