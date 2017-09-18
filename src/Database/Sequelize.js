"use strict";

var Sequelize = require('sequelize');

exports.sequelizeImpl = function sequelizeImpl (ps) {
  var database = ps.database;
  var username = ps.username;
  var password = ps.password;
  console.log("args",ps);
  delete ps.database;
  delete ps.username;
  delete ps.password;
  return new Sequelize(database,username,password,ps);
};

exports.authenticateImpl = function authenticateImpl (onError,onSuccess,sequelize) {
  sequelize.authenticate().then(onSuccess).catch(onError);
};

exports.syncImpl = function syncImpl (onError,onSuccess,sequelize) {
  sequelize.sync().then(onSuccess).catch(onError);
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

var inflect = require('inflect');

exports.belongsToImpl = function belongsToImpl (childName,child,parent) {
  child.belongsTo(parent);
  return {
    get: function getImpl (onError,onSuccess) {
      parent['get' + inflect.capitalize(childName)]().then(onSuccess).catch(onError);
    },
    set: function setImpl (onError,onSuccess,x) {
      parent['set' + inflect.capitalize(childName)](x).then(onSuccess).catch(onError);
    }
  };
};

exports.hasOneImpl = function hasOneImpl (parent,child,childName) {
  parent.hasOne(child);
  return {
    get: function getImpl (onError,onSuccess) {
      parent['get' + inflect.capitalize(childName)]().then(onSuccess).catch(onError);
    },
    set: function setImpl (onError,onSuccess,x) {
      parent['set' + inflect.capitalize(childName)](x).then(onSuccess).catch(onError);
    }
  };
};

exports.hasManyImpl = function hasManyImpl (parent,child,childName) {
  parent.hasMany(child);
  return {
    get: function getImpl (onError,onSuccess) {
      parent['get' + inflect.capitalize(inflect.pluralize(childName))]().then(onSuccess).catch(onError);
    },
    set: function setImpl (onError,onSuccess,xs) {
      parent['set' + inflect.capitalize(inflect.pluralize(childName))](xs).then(onSuccess).catch(onError);
    },
    add: function addImpl (onError,onSuccess,xs) {
      parent['add' + inflect.capitalize(inflect.pluralize(childName))](xs).then(onSuccess).catch(onError);
    },
    has: function hasImpl (onError,onSuccess,xs) {
      parent['has' + inflect.capitalize(inflect.pluralize(childName))](xs).then(onSuccess).catch(onError);
    },
    remove: function removeImpl (onError,onSuccess,xs) {
      parent['remove' + inflect.capitalize(inflect.pluralize(childName))](xs).then(onSuccess).catch(onError);
    }
  };
};

exports.belongsToManyImpl = function belongsToManyImpl (childName,child,parent,through) {
  child.belongsToMany(parent,through);
  return {
    get: function getImpl (onError,onSuccess) {
      parent['get' + inflect.capitalize(inflect.pluralize(childName))]().then(onSuccess).catch(onError);
    },
    set: function setImpl (onError,onSuccess,xs,through) {
      parent['set' + inflect.capitalize(inflect.pluralize(childName))](xs,through).then(onSuccess).catch(onError);
    },
    add: function addImpl (onError,onSuccess,xs,through) {
      parent['add' + inflect.capitalize(inflect.pluralize(childName))](xs,through).then(onSuccess).catch(onError);
    },
    has: function hasImpl (onError,onSuccess,xs) {
      parent['has' + inflect.capitalize(inflect.pluralize(childName))](xs).then(onSuccess).catch(onError);
    },
    remove: function removeImpl (onError,onSuccess,xs) {
      parent['remove' + inflect.capitalize(inflect.pluralize(childName))](xs).then(onSuccess).catch(onError);
    }
  };
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
