package com.hunteryavitz.blockchainapi.entities.healthmetric;

import com.google.gson.Gson;
import lombok.Data;

/**
 * The HealthMetricProductionModel class is a model for the production health metric.
 */
@Data
public class HealthMetricProductionModel {

    /**
     * The data for the production health metric.
     */
    private Data data;

    /**
     * The constructor for the HealthMetricProductionModel class.
     */
    public HealthMetricProductionModel() {
        data = new Data();
    }

    /**
     * Sets the data for the production health metric.
     * @param data the data for the production health metric
     */
    public void setDataset1(int[] data) {
        this.data.setData1(data);
    }

    /**
     * Sets the data for the production health metric.
     * @param data the data for the production health metric
     */
    public void setDataset2(int[] data) {
        this.data.setData2(data);
    }

    /**
     * The Data class is a model for the data of the production health metric.
     */
    static class Data {

        /**
         * The data for the transactions health metric.
         */
        private int[] dataset1;

        /**
         * The data for the blocks health metric.
         */
        private int[] dataset2;

        /**
         * Sets the data for the production health metric.
         * @param data the data for the production health metric
         * */
        public void setData1(int[] data) {
            this.dataset1 = data;
        }

        /**
         * Sets the data for the production health metric.
         * @param data the data for the production health metric
         */
        public void setData2(int[] data) {
            this.dataset2 = data;
        }
    }

    /**
     * Returns the data as a JSON string.
     * @return the data as a JSON string
     */
    public String asJson() {
        Gson gson = new Gson();
        return gson.toJson(this);
    }
}
