package lib

import (
    "log"
    "github.com/spf13/viper"
)

// Env has environment stored
type Env struct {
    ServerPort  string `mapstructure:"SERVER_PORT"`
    Environment string `mapstructure:"ENV"`
    LogOutput   string `mapstructure:"LOG_OUTPUT"`
    LogLevel    string `mapstructure:"LOG_LEVEL"`
}

// NewEnv creates a new environment
func NewEnv() Env {

    env := Env{}

    viper.SetDefault("SERVER_PORT", "8080")
    viper.SetDefault("ENV", "production")
    viper.SetDefault("LOG_OUTPUT", "")
    viper.SetDefault("LOG_LEVEL", "warn")

    viper.AutomaticEnv()

    err := viper.Unmarshal(&env)
    if err != nil {
        log.Fatal("☠️ environment can't be loaded: ", err)
    }

    return env
}
