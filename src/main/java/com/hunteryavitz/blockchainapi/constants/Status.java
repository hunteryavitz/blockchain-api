package com.hunteryavitz.blockchainapi.constants;

/**
 * The Status enum represents the status of an order.
 */
public enum Status {
    /**
     * The order has been created.
     */
    CREATED,
    /**
     * The order is pending.
     */
    PENDING,

    /**
     * The order has been fulfilled.
     */
    FULFILLED,

    /**
     * The order has been rejected.
     */
    REJECTED,

    /**
     * The order has been cancelled.
     */
    CANCELLED,

    /**
     * The order has been shipped.
     */
    SHIPPED,

    /**
     * The order has been delivered.
     */
    DELIVERED,

    /**
     * The order has been returned.
     */
    RETURNED,

    /**
     * The order has been refunded.
     */
    REFUNDED,
    /**
     * The order has been received.
     */
    RECEIVED,

    /**
     * The order has been completed.
     */
    COMPLETED
}
