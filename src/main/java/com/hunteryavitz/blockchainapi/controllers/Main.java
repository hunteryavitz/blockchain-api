package com.hunteryavitz.blockchainapi.controllers;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Main controller for the API.
 */
@RestController
@CrossOrigin
@RequestMapping("/api/v1")
public class Main {

    /**
     * Returns a 200 response to indicate that the API is ready to accept requests.
     * @return 200 response
     */
    @GetMapping("/readiness")
    public ResponseEntity<Boolean> isReady() {
        return ResponseEntity.ok(true);
    }

    /**
     * Returns a 200 response to indicate that the API is alive.
     * @return 200 response
     */
    @GetMapping("/liveness")
    public ResponseEntity<Boolean> isAlive() {
        return ResponseEntity.ok(true);
    }

    /**
     * Returns the version of the API.
     * @return 200 response with the version of the API
     */
    @GetMapping("/version")
    public ResponseEntity<String> getVersion() {
        return ResponseEntity.ok("0.0.1");
    }
}
