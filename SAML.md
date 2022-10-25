# SAML

## Getting started

* To start an example keycloak instance, run `docker-compose up -d` in the SAML folder

## Keycloak configuration
### Creating client
 * Configure -> Clients -> Create
 * Client id: authservice (or the name of the SP)
   * This has to match the `SAML_AUDIENCE` setting
 * Client protocol: SAML

### Minimal changes from default settings
 * root url (e.g. `http://localhost:8000/` for the example)
 * Client Signature required: OFF
   * wai-saml2 doesn't handle client requests
   * I haven't implement request signing
   * Leads to error: "Invalid requester
 * Encrypt Assertions: ON
   * Required by wai-saml2
   * See https://github.com/mbg/wai-saml2/issues/5
   * After saving the settings, go to new tab "keys" and generate or import encryption keys
   * (Download and install the encryption key)

### Installing encryption key
  * *NOTE*: Simplified installation can be implemented
  * Go to client settings, keys, under Encryption keys
  * Copy private key text, store in SAML/secrets/privkey.pem
  * Add Private key PEM header:
```
-----BEGIN RSA PRIVATE KEY-----
<copied private key>
-----END RSA PRIVATE KEY-----
```

### Getting the realm certificate
  * *NOTE*: Simplified installation can be implemented
  * Realm Settings
  * Tab "Keys" -> "Active"
  * Under "Algorithm": `RS256`, "Use": `Sig`
  * Chose "Certificate", copy/paste and save under SAML/secrets/realm.cert
  * Add PEM markers:
```
-----BEGIN CERTIFICATE-----
<copied certificate data>
-----END CERTIFICATE-----
```
### Setting up user attributes
auth-service requires some attributes to function, you can set them up in keycloak:
  * under clients -> auth-service -> Mappers, then for each of them create
  * name: email
    * Mapper type: User Property
    * Property: email
    * SAMl Attribute name: email
  * name: User name
    * Mapper type: Javascript Mapper
    * Script: `user.getFirstName() + " " + user.getLastName()`
    * SAML Attribute name: name
  * Make sure that all users have email and name set (keycloak admin by default does not)

## Configuring auth-service
  The following configuration options are required
  * SAML_ENCRYPTION_PRIVATE_KEY_PATH: Path to pem-encoded RSA encryption private key
  * SAML_SIGNING_CERTIFICATE_PATH: Path to pem-encoded X509 certificate for IP
    signature verification
  * SAML_AUDIENCE: Client ID (has to be the same as configured in keycloak)
  * SAML_IP_BASE_URL: URL of the IP request end point
  See also docker-compose.yml for example

## Start auth-service
  * E.g. `make up && docker-compose logs --tail=50 --follow`

## Testing SAML SSO:

If the example server is running, you can navigate your browser to
`http://localhost:8000/api/sso/login` to test the SSO login
