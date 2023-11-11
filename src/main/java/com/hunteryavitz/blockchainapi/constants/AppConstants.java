package com.hunteryavitz.blockchainapi.constants;

import lombok.Getter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * Constants for the application.
 */
@Component
@Getter
public class AppConstants {

    /**
     * The node manager registry address.
     */
    @Value("${REGISTRY_ADDRESS}")
    private String registryAddress;

    /**
     * The node certificate.
     */
    @Value("${CERTIFICATE}")
    private String certificate;

    /**
     * The node port.
     */
    @Value("${PORT}")
    private int port;
}
