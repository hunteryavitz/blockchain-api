package com.hunteryavitz.blockchainapi.entities;

import com.hunteryavitz.blockchainapi.constants.TransactionStatus;
import lombok.Data;

/**
 * The Transaction class represents a transaction in the blockchain.
 */
@Data
public class Transaction {

    /**
     * The id of the transaction.
     */
    private long id;

    /**
     * The timestamp of the transaction.
     */
    private String timestamp;

    /**
     * The source of the transaction.
     */
    private String source;

    /**
     * The status of the transaction.
     */
    private TransactionStatus transactionStatus;

    /**
     * The constructor for the Transaction class.
     * @param id The id of the transaction.
     * @param timestamp The timestamp of the transaction.
     * @param source The source of the transaction.
     * @param transactionStatus The status of the transaction.
     */
    public Transaction(long id, String timestamp, String source, String transactionStatus) {
        this.id = id;
        this.timestamp = timestamp;
        this.source = source;
        this.transactionStatus = Enum.valueOf(TransactionStatus.class, transactionStatus);
    }
}
