"use strict";

var Sequelize = require('sequelize');

exports.sequelizeImpl = function sequelizeImpl (ps) {
  var database = ps.database;
  var username = ps.username;
  var password = ps.password;
  delete ps.database;
  delete ps.username;
  delete ps.password;
  return new Sequelize(database,username,password,ps);
};

exports.authenticateImpl = function authenticateImpl (onError,onSuccess,sequelize) {
  sequelize.authenticate().then(onSuccess).catch(onError);
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

exports.defineImpl = function defineImpl (sequelize,modelName,fields) {
  return sequelize.define(modelName,fields);
};

exports.findByIdImpl = function findByIdImpl (onError,onSuccess,Model,id) {
  Model.findById(id).then(onSuccess).catch(onError);
};

exports.findOneImpl = function findOneImpl (onError,onSuccess,Model,ps) {
  Model.findOne(ps).then(onSuccess).catch(onError);
};

exports.findAllImpl = function findOneImpl (onError,onSuccess,Model,ps) {
  Model.findAll(ps).then(onSuccess).catch(onError);
};

exports.buildImpl = function buildImpl (Model,fields) {
  return Model.build(fields);
};

exports.saveImpl = function saveImpl (onError,onSuccess,value) {
  value.save().then(onSuccess).catch(onError);
};

exports.createImpl = function createImpl (onError,onSuccess,Model,fields) {
  Model.create(fields).then(onSuccess).catch(onError);
};

exports.bulkCreateImpl = function bulkCreateImpl (onError,onSuccess,Model,fields) {
  Model.bulkCreate(fields).then(onSuccess).catch(onError);
};

exports.updateImpl = function updateImpl (onError,onSuccess,value,fields) {
  value.update(fields).then(onSuccess).catch(onError);
};

exports.destroyImpl = function destroyImpl (onError,onSuccess,value) {
  value.destroy().then(onSuccess).catch(onError);
};

exports.getImpl = function getImpl (value,getParams) {
  return value.get(getParams);
};
