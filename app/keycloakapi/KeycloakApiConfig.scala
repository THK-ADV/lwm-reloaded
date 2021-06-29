package keycloakapi

case class KeycloakApiConfig(
    keycloakBaseUrl: String,
    realm: String,
    adminCliClientId: String,
    adminCliClientSecret: String
)
