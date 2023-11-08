package com.hunteryavitz.blockchainapi.constants;

import lombok.Getter;
import org.springframework.beans.factory.annotation.Value;

/**
 * Constants for the application.
 */
public class AppConstants {

    /**
     * The node manager registry address.
     */
    @Value("${REGISTRY_ADDRESS}")
    @Getter
    private static String registryAddress;

    /**
     * The node certificate.
     */
    @Value("${CERTIFICATE}")
    @Getter
    private static String certificate;

    /**
     * The node port.
     */
    @Value("${PORT}")
    @Getter
    private static int port;
}
