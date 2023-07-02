package com.hunteryavitz.blockchainapi;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * The BlockchainApiApplication class is the main class for the blockchain-api
 * application. It is responsible for starting the Spring Boot application.
 */
@SpringBootApplication
public class BlockchainApiApplication {

    /**
     * The main method is responsible for starting the Spring Boot application.
     * @param args The command line arguments.
     */
    public static void main(String[] args) {
        SpringApplication.run(BlockchainApiApplication.class, args);
    }

}
