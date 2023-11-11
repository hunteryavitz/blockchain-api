# Blockchain API
**Last Updated:** 11/11/23

This is the API and backend code for the blockchain project.

### Objective
This back end serves as the API for the blockchain project.

### Details
This API will service requests from the client-side of this project.

The remote server is equipped with an SSL connection.

**Build and Compile**
```shell
mvn package
```

**Run Unit Tests**
```shell
mvn test
```

**Generate Javadocs**
```shell
mvn javadoc:javadoc
```

## Endpoints
The current serviceable endpoints are as follows:

###
GET http://localhost:8080/api/v1/blockchain/verifyBlockchain

###
GET http://localhost:8080/api/v1/blockchain/getBlockchain

###
GET http://localhost:8080/api/v1/blockchain/getBlockById

###
POST http://localhost:8080/api/v1/block/addBlockToBlockchain
Content-Type: application/x-www-form-urlencoded

###
GET http://localhost:8080/api/v1/healthMetric/updateProductionHealth

###
GET http://localhost:8080/api/v1/healthMetric/getProductionHealth

###
GET http://localhost:8080/api/v1/healthMetric/getExceptionHealth

###
GET http://localhost:8080/api/v1/readiness

###
GET http://localhost:8080/api/v1/liveness

###
GET http://localhost:8080/api/v1/version

###
GET http://localhost:8080/api/v1/node/getNodeStatus

###
GET http://localhost:8080/api/v1/node/getNodeTraffic

###
POST http://localhost:8080/api/v1/nodeManager/registerNode
Content-Type: application/json

{}

###
GET http://localhost:8080/api/v1/nodeManager/getNodeNetworkStatus

###
GET http://localhost:8080/api/v1/nodeManager/nodeNetworkRollCall

###
POST http://localhost:8080/api/v1/transaction/submitTransaction
Content-Type: application/json

{}

###
GET http://localhost:8080/api/v1/transaction/getTransactionPool

---

### Summary
The aim of this API is to provide service to the client-side.

### Related
This API is a component of the [Blockchain Project](https://github.com/hunteryavitz/blockchain-main).  It works as the back-end code to provide services to the following clients:

- [Blockchain Admin Client](https://github.com/hunteryavitz/blockchain-client-admin)
- [Blockchain Mobile Client](https://github.com/hunteryavitz/blockchain-client-mobile)

### Extra
Feel free to download, modify, or use in any way.

Developer: [Hunter Yavitz](mailto:h.yavitz@gmail.com)
