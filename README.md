# Blockchain API

[![Github status](https://github.com/hunteryavitz/blockchain-api/actions/workflows/ci-cd.yml/badge.svg)]()

[![Github forks](https://img.shields.io/github/forks/hunteryavitz/blockchain-api.svg)](https://github.com/hunteryavitz/blockchain-api/network)
[![Github stars](https://img.shields.io/github/stars/hunteryavitz/blockchain-api.svg)](https://github.com/hunteryavitz/blockchain-api/stargazers)

**Last Updated:** 11/28/23

This is the API and backend code for the [Blockchain Project](https://github.com/hunteryavitz/blockchain-main).

### Objective

---

This back end serves as the API for the [Blockchain Project](https://github.com/hunteryavitz/blockchain-main).

### Details

---

This API will service requests from the 
[Blockchain Admin Web Client](https://github.com/hunteryavitz/blockchain-client-admin) and 
[Blockchain Field Mobile Client](https://github.com/hunteryavitz/blockchain-client-mobile).

This API is currently a *hybrid* of the **Node API** and the **Node Manager API**, which will be decoupled in upcoming
development.

As-is, this API can be fully utilized by running separate instances loaded with different property configurations.
These instance groups should contain exactly *one* **Node Manager API** instance running from `8080` and *one or more* 
**Node API** instance(s) running from `9000` and up.

The ports can be set in the `application.properties` file.

### Usage

---

Follow the instructions below to run the API locally.

- Start an instance of the **Node Manager API** on port `8080`.
- Start one or more instances of the **Node API** on ports `9000` and up.
  - These instances will attempt to register themselves to the **Node Manager API** on startup.

### Development

---

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

### Resources

---

- Export the [Blockchain API Collection](docs/postman/Blockchain%20API%20v0.0.19.postman_collection.json)
to [Postman](https://www.postman.com/) for testing and development.

- Review the [Endpoints](docs/ENDPOINTS.md) for a list of current available endpoints.

### Http Requests

---

- Execute requests locally in IDE using [ENDPOINTS.http](docs/ENDPOINTS.http).

### Summary

---

The aim of this API is to provide service to the client-side structure.

### Related
This API is a component of the [Blockchain Project](https://github.com/hunteryavitz/blockchain-main).  It works as the back-end code to provide services to the following clients:

- [Blockchain Admin Web Client](https://github.com/hunteryavitz/blockchain-client-admin)
- [Blockchain Field Mobile Client](https://github.com/hunteryavitz/blockchain-client-mobile)

### Extra
Feel free to download, modify, or use in any way.

Feedback welcome.

---

Developer: [Hunter Yavitz](mailto:h.yavitz@gmail.com)
