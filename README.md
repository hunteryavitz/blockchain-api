# Blockchain API
**Last Updated:** 7/8/23

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

### Endpoints
The current serviceable endpoints are as follows:

**Readiness Check**
- [localhost:8080/api/v1/readiness](http://localhost:8080/api/v1/readiness)

**Liveness Check**
- [localhost:8080/api/v1/liveness](http://localhost:8080/api/v1/liveness)

**Version Check**
- [localhost:8080/api/v1/version](http://localhost:8080/api/v1/version)

**Verify Blockchain**
- [localhost:8080/api/v1/verifyBlockchain](http://localhost:8080/api/v1/verifyBlockchain)

**Add Block to Blockchain**
- [localhost:8080/api/v1/block/addBlockToBlockchain](http://localhost:8080/api/v1/block/addBlockToBlockchain)

**Verify Blockchain**
- [localhost:8080/api/v1/blockchain/verifyBlockchain](http://localhost:8080/api/v1/blockchain/verifyBlockchain)

**Get Blockchain**
- [localhost:8080/api/v1/blockchain/getBlockchain](http://localhost:8080/api/v1/blockchain/getBlockchain)

### Summary
The aim of this API is to provide service to the client-side.

### Related
This API is a component of the [Blockchain Project](https://github.com/hunteryavitz/blockchain-main).  It works as the back-end code to provide services to the following clients:

- [Blockchain Admin Client](https://github.com/hunteryavitz/blockchain-client-admin)
- [Blockchain Mobile Client](https://github.com/hunteryavitz/blockchain-client-mobile)

### Extra
Feel free to download, modify, or use in any way.

Developer: [Hunter Yavitz](mailto:h.yavitz@gmail.com)
