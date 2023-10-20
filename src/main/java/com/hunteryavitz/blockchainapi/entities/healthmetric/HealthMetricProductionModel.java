package com.hunteryavitz.blockchainapi.entities.healthmetric;

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
        private int[] dataset1;
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
        return "{" +
                "\"data\": {" +
                "\"dataset1\": " + asJsonArray(data.dataset1) + "," +
                "\"dataset2\": " + asJsonArray(data.dataset2) +
                "}" +
                "}";
    }

    /**
     * Returns the data as a JSON array.
     * @param dataset1 the data to be converted to a JSON array
     * @return the data as a JSON array
     */
    private String asJsonArray(int[] dataset1) {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (int i = 0; i < dataset1.length; i++) {
            sb.append(dataset1[i]);
            if (i != dataset1.length - 1) {
                sb.append(",");
            }
        }
        sb.append("]");
        return sb.toString();
    }
}
