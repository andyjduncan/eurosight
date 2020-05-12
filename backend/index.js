'use strict';

const AWS = require('aws-sdk');

const addVoter = async (connectionId) => {
    const dynamo = new AWS.DynamoDB.DocumentClient();

    const tableName = process.env.TABLE_NAME;

    const params = {
        Key: {Event: 'test', Type: 'voters'},
        TableName: tableName,
        AttributeUpdates: {
            Voters: {
                Value: dynamo.createSet([connectionId]),
                Action: 'ADD'
            }
        }
    };
    return dynamo.update(
        params
    ).promise()
};

const removeVoter = async (connectionId) => {
    const dynamo = new AWS.DynamoDB.DocumentClient();

    const tableName = process.env.TABLE_NAME;

    const params = {
        Key: {Event: 'test', Type: 'voters'},
        TableName: tableName,
        AttributeUpdates: {
            Voters: {
                Value: dynamo.createSet([connectionId]),
                Action: 'DELETE'
            }
        }
    };
    return dynamo.update(
        params
    ).promise()
};

const connect = async (event) => {
    console.log(JSON.stringify(event));

    try {
        await addVoter(event.requestContext.connectionId);
        return {
            statusCode: 200
        };
    } catch (err) {
        console.log(err);
        return {
            statusCode: 502
        }
    }
};

const disconnect = async (event) => {
    console.log(JSON.stringify(event));

    try {
        await removeVoter(event.requestContext.connectionId);
        return {
            statusCode: 200
        };
    } catch (err) {
        console.log(err);
        return {
            statusCode: 502
        }
    }
};

const vote = (event) => {
    console.log(JSON.stringify(event));
};

const processChange = (event) => {
    console.log(JSON.stringify(event));
};

module.exports = {connect, disconnect, vote, processChange};