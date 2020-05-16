'use strict';

const countries = {
    ad: 'Andorra',
    al: 'Albania',
    am: 'Armenia',
    at: 'Austria',
    au: 'Australia',
    az: 'Azerbaijan',
    ba: 'Bosnia And Herzegovina',
    bg: 'Bulgaria',
    be: 'Belgium',
    by: 'Belarus',
    ch: 'Switzerland',
    cy: 'Cyprus',
    cz: 'Czech Republic',
    de: 'Germany',
    dk: 'Denmark',
    ee: 'Estonia',
    es: 'Spain',
    fi: 'Finland',
    fr: 'France',
    ge: 'Georgia',
    gr: 'Greece',
    hr: 'Croatia',
    hu: 'Hungary',
    ie: 'Ireland',
    il: 'Israel',
    is: 'Iceland',
    it: 'Italy',
    lt: 'Lithuania',
    lu: 'Luxemburg',
    lv: 'Latvia',
    ma: 'Morocco',
    mc: 'Monaco',
    md: 'Moldova',
    me: 'Montenegro',
    mt: 'Malta',
    mk: 'The Former Yugoslav Republic Of Macedonia',
    nl: 'Netherlands',
    no: 'Norway',
    pl: 'Poland',
    pt: 'Portugal',
    ro: 'Romania',
    rs: 'Serbia',
    ru: 'Russia',
    se: 'Sweden',
    si: 'Slovenia',
    sk: 'Slovakia',
    sm: 'San Marino',
    tr: 'Turkey',
    ua: 'Ukraine',
    uk: 'United Kingdom'
};

const performingCountries = [
    'nl',
    'de',
    'es',
    'cz',
    'ch',
    'dk',
    'pl',
    'it',
    'ua',
    'is',
    'ro',
    'se',
    'gr',
    'fi',
    'uk',
    'ru',
    'no',
    'ie'
];

const AWS = require('aws-sdk');

const tableName = process.env.TABLE_NAME;

const dynamo = new AWS.DynamoDB.DocumentClient();

const shuffle = (a) => {
    for (let i = a.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1));
        [a[i], a[j]] = [a[j], a[i]];
    }
    return a;
};

const addConnection = async (connectionId) => {
    const params = {
        TableName: tableName,
        Item: {
            Type: 'connection',
            Key: connectionId
        }
    };
    return dynamo.put(params).promise();
};

const removeConnection = async (connectionId) => {
    const params = {
        TableName: tableName,
        Key: {
            Type: 'connection',
            Key: connectionId
        }
    };
    return dynamo.delete(params).promise();
};

const sendMessage = async (connectionId, message) => {
    const apiGateway = new AWS.ApiGatewayManagementApi({
        endpoint: process.env.ENDPOINT_URL
    });

    try {
        return apiGateway.postToConnection({
            ConnectionId: connectionId,
            Data: JSON.stringify(message)
        }).promise();
    } catch (err) {
        if (err.statusCode === 410) {
            return removeConnection(connectionId);
        } else {
            console.log(err);
        }
    }
};

const sendMessageToAll = async (message) => {
    const params = {
        TableName: tableName,
        KeyConditionExpression: '#type = :type',
        ExpressionAttributeNames: {
            '#type': 'Type',
        },
        ExpressionAttributeValues: {
            ':type': 'connection',
        }
    };
    const connections = await dynamo.query(params).promise();

    return Promise.all(connections.Items.map((v) => sendMessage(v.Key, message)));
};

const getAllCountries = async () => {
    const params = {
        TableName: tableName,
        KeyConditionExpression: '#type = :type',
        ExpressionAttributeNames: {
            '#type': 'Type',
        },
        ExpressionAttributeValues: {
            ':type': 'country',
        }
    };
    const countries = await dynamo.query(params).promise();

    return countries.Items.map((c) => c.Key);
};

const connect = async (event) => {
    try {
        await addConnection(event.requestContext.connectionId);
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
    try {
        await removeConnection(event.requestContext.connectionId);
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

const init = async (event) => {
    const message = JSON.parse(event.body);

    try {
        const connectionId = event.requestContext.connectionId;
        if (message.country) {
            await updateCountry(message.country, connectionId);
        } else {
            const newCountry = await addCountry(connectionId);
            if (!newCountry) {
                await sendMessage(connectionId, {"event": "noCountries"});
            }
        }
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

const refresh = async (event) => {
    try {
        const country = await findCountry(event.requestContext.connectionId);
        await countryInit(country);
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

const makeAdmin = async (event) => {
    const message = JSON.parse(event.body);

    try {
        const params = {
            TableName: tableName,
            Key: {
                Type: 'country',
                Key: message.country
            },
            UpdateExpression: `SET #admin = :isAdmin`,
            ExpressionAttributeNames: {
                '#admin': 'IsAdmin'
            },
            ExpressionAttributeValues: {
                ':isAdmin': true
            }
        };
        await dynamo.update(params).promise();
        return {
            statusCode: 202
        };
    } catch (err) {
        console.log(err);
        return {
            statusCode: 502
        }
    }
};

const addCountry = async (connectionId) => {
    const candidates = shuffle([...Object.keys(countries)]);
    while (candidates.length > 0) {
        const candidate = candidates.pop();
        const params = {
            TableName: tableName,
            Item: {
                Type: 'country',
                Key: candidate,
                ConnectionId: connectionId
            },
            ConditionExpression: 'attribute_not_exists(#type) and attribute_not_exists(#key)',
            ExpressionAttributeNames: {
                '#type': 'Type',
                '#key': 'Key'
            }
        };
        try {
            await dynamo.put(params).promise();
            return candidate;
        } catch (err) {
            if (err.code !== 'ConditionalCheckFailedException') {
                throw err;
            }
        }
    }
};

const updateCountry = async (country, connectionId) => {
    const params = {
        TableName: tableName,
        Key: {
            Type: 'country',
            Key: country
        },
        UpdateExpression: `SET #connectionId = :connectionId`,
        ExpressionAttributeNames: {
            '#connectionId': 'ConnectionId'
        },
        ExpressionAttributeValues: {
            ':connectionId': connectionId
        }
    };
    return dynamo.update(params).promise();
};

const findCountry = async (connectionId) => {
    const params = {
        TableName: tableName,
        KeyConditionExpression: '#type = :type',
        FilterExpression: '#con = :connectionId',
        ExpressionAttributeNames: {
            '#type': 'Type',
            '#con': 'ConnectionId'
        },
        ExpressionAttributeValues: {
            ':type': 'country',
            ':connectionId': connectionId
        }
    };
    const countries = await dynamo.query(params).promise();
    if (countries.Count !== 1) {
        throw new Error(`Couldn't find country for ${connectionId}`);
    }
    return countries.Items[0];
};

const addScores = async (country, scores) => {
    const scoresByCountry = scores.reduce((acc, x) => ({...acc, ...x}), {});
    const operations = Object.entries(scoresByCountry)
        .reduce((acc, [k, v]) => ({
                ops: [`#${k} = :${k}Val`, ...acc.ops],
                names: {[`#${k}`]: `${k}`, ...acc.names},
                values: {
                    [`:${k}Val`]: v, ...acc.values
                }
            }),
            {ops: [], names: {}, values: {}});
    const params = {
        TableName: tableName,
        Key: {Type: 'scores', Key: country},
        UpdateExpression: `SET ${operations.ops.join(', ')}`,
        ExpressionAttributeNames: operations.names,
        ExpressionAttributeValues: operations.values
    };
    return dynamo.update(
        params
    ).promise();
};

const enableVoting = async (event) => {
    const message = JSON.parse(event.body);

    try {
        const params = {
            TableName: tableName,
            Key: {
                Type: 'country',
                Key: message.country
            },
            UpdateExpression: `SET #votingEnabled = :isVotingEnabled`,
            ExpressionAttributeNames: {
                '#votingEnabled': 'IsVotingEnabled'
            },
            ExpressionAttributeValues: {
                ':isVotingEnabled': true
            }
        };
        await dynamo.update(params).promise();
        return {
            statusCode: 202
        };
    } catch (err) {
        console.log(err);
        return {
            statusCode: 502
        }
    }
};

const vote = async (event) => {
    try {
        const country = await findCountry(event.requestContext.connectionId);
        const voteEvent = JSON.parse(event.body);
        await addScores(country.Key, voteEvent.scores);
        return {
            statusCode: 202
        }
    } catch (err) {
        console.log(err);
        return {
            statusCode: 500
        }
    }
};

const countryPerformance = async (event) => {
    try {
        const performanceEvent = JSON.parse(event.body);

        const params = {
            TableName: tableName,
            Item: {
                Type: 'performance',
                Key: Date.now().toString(),
                Country: performanceEvent.country
            }
        };
        await dynamo.put(params).promise();
        return {
            statusCode: 202
        }
    } catch (err) {
        console.log(err);
        return {
            statusCode: 500
        }
    }
};

const currentScores = async () => {
    const params = {
        TableName: tableName,
        KeyConditionExpression: '#type = :type',
        ExpressionAttributeNames: {
            '#type': 'Type',
        },
        ExpressionAttributeValues: {
            ':type': 'scores',
        }
    };
    const scoresResponse = await dynamo.query(params).promise();
    const mapped = scoresResponse.Items
        .reduce((acc, x) => {
            Object.entries(x).forEach(([k, v]) => {
                if (acc.hasOwnProperty(k)) {
                    acc[k] += v;
                } else {
                    acc[k] = v;
                }
            });
            return acc;
        }, {});
    const {Type, Key, ...scores} = mapped;
    return scores;
};

const updateScores = async () => {
    const scores = await currentScores();
    return sendMessageToAll({event: 'scores', scores});
};

const getPerformances = async () => {
    const params = {
        TableName: tableName,
        KeyConditionExpression: '#type = :type',
        ExpressionAttributeNames: {
            '#type': 'Type',
        },
        ExpressionAttributeValues: {
            ':type': 'performance',
        },
        ScanIndexForward: false
    };
    const performancesResponse = await dynamo.query(params).promise();
    return performancesResponse.Items.map((p) => p.Country);
};

const countryInit = async (country) => {
    const connectionId = country.ConnectionId;
    await sendMessage(connectionId, {event: 'country', country: country.Key});
    await sendMessage(connectionId, {event: 'allCountries', countries});
    const performedCountries = await getPerformances();
    await sendMessage(connectionId, {event: 'performedCountries', countries: performedCountries});
    const scores = await currentScores();
    await sendMessage(connectionId, {event: 'scores', scores});
    if (country.IsVotingEnabled) {
        await sendMessage(connectionId, {event:'votingEnabled'})
    }
    if (country.IsAdmin) {
        await adminInit(country);
    }
};

const adminInit = async (country) => {
    const connectionId = country.ConnectionId;
    await sendMessage(connectionId, {event: 'madeAdmin'});
    await sendMessage(connectionId, {event: 'performingCountries', countries: performingCountries});
    const countries = await getAllCountries();
    await sendMessage(connectionId, {event: 'votingPanels', countries})
};

const updatePerformances = async () => {
    const countries = await getPerformances();
    await sendMessageToAll({event: "performedCountries", countries});
};

const processChange = async (event) => {
    try {
        await Promise.all(event.Records.map((e) => {
            const keys = AWS.DynamoDB.Converter.unmarshall(e.dynamodb.Keys);
            const newImage = AWS.DynamoDB.Converter.unmarshall(e.dynamodb.NewImage);
            switch (keys.Type) {
                case "connection":
                    return;
                case "country":
                    return countryInit(newImage);
                case "scores":
                    return updateScores();
                case "performance":
                    return updatePerformances();

                default:
                    return Promise.resolve();
            }
        }));
    } catch (err) {
        console.log(err);
    }
};

module.exports = {connect, disconnect, init, refresh, makeAdmin, enableVoting, vote, countryPerformance, processChange};