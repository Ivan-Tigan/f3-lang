# LeanCMS Documentation

**So simple, even a dummy can use it.**

LeanCMS is an ultra-lightweight content management system built on F3 logic programming. It automatically generates CRUD interfaces and JSON APIs from simple table specifications.

## Quick Start

```bash
# Pull and run with basic config
docker run -p 8080:8080 -e F3_CONFIG='CMSSpec tables { blog title text; content html; published date. }; login "admin":"password"; apiKey "your-secret-key"; uploadsURLPath "uploads". system runWebServer 8080.' ivantsoninski/leancms:latest

# Or with docker-compose (see example below)
docker-compose up -d
```

Access the web interface at `http://localhost:8080`

## Table Specification

Define your content structure using the simple F3 syntax:

```f3
CMSSpec tables {
    blog slug text; title text; content html; published date.
    faq question text; answer html; order number.
    config key text; value text.
}; login "admin":"password"; apiKey "your-secret-key"; uploadsURLPath "uploads".
system runWebServer 3000.
```

### Field Types

- `text` - Text input field
- `html` - Rich HTML editor (TinyMCE)
- `date` - Date picker
- `number` - Number input

### Configuration Structure

The F3_CONFIG contains two parts:
1. **CMSSpec** - Defines tables, authentication, API keys, and upload settings
2. **Server startup** - `system runWebServer 3000` starts the web server

#### CMSSpec Parameters

- **tables** - Define content structure with field types
- **login** - Set admin username and password (`login "admin":"password"`)
- **apiKey** - Set API key for authentication (`apiKey "your-secret-key"`)
- **uploadsURLPath** - Set URL path for file uploads (`uploadsURLPath "uploads"`)
  - Files uploaded through the rich editor will be accessible at `/uploads/filename.jpg`
  - Maps to the `/data/uploads` volume mount in Docker

This allows complete customization through the F3_CONFIG environment variable alone.

## Authentication

LeanCMS supports two authentication methods:

### Basic Authentication

```bash
curl -u admin:password http://localhost:3000/table/blog
```

### API Key (Bearer Token)

```bash
curl -H "Authorization: Bearer your-secret-key" http://localhost:3000/table/blog
```

## Web Interface

The web interface automatically generates:

- **Table listing** - View all defined tables at `/`
- **CRUD operations** - Create, read, update, delete for each table
- **Pagination** - Configurable rows per page (10, 25, 50)
- **Rich editing** - HTML fields use TinyMCE editor
- **File uploads** - Image upload support for rich content

## JSON API

LeanCMS automatically exposes JSON APIs for all defined tables.

### Table Specification → API Endpoints

Given this specification:

```f3
CMSSpec tables {
    blog slug text; title text; content html; published date.
    products name text; price number; description html.
}.
```

The following JSON endpoints are automatically available:

#### List Table Rows

```http
GET /table/{tablename}
Accept: application/json
Authorization: Bearer your-secret-key
```

**Examples:**

```bash
# List blog posts
curl -H "Accept: application/json" -H "Authorization: Bearer key" \
  http://localhost:3000/table/blog

# List products with pagination
curl -H "Accept: application/json" -H "Authorization: Bearer key" \
  http://localhost:3000/table/products?page=0&rows=25
```

**Response Format:**

```json
[
  {
    "id": "blog_abc123",
    "slug": "my-first-post",
    "title": "My First Post",
    "content": "<p>Hello world!</p>",
    "published": "2025-01-15"
  },
  {
    "id": "blog_def456",
    "slug": "second-post",
    "title": "Another Post",
    "content": "<p>More content...</p>",
    "published": "2025-01-16"
  }
]
```

### Query Parameters

| Parameter | Description           | Default |
| --------- | --------------------- | ------- |
| `page`    | Page number (0-based) | `0`     |
| `rows`    | Rows per page         | `10`    |

### Examples by Table Type

#### Blog Table

```f3
blog slug text; title text; content html; published date.
```

- **Endpoint:** `GET /table/blog`
- **Fields:** `id`, `slug`, `title`, `content`, `published`

#### FAQ Table

```f3
faq question text; answer html; order number.
```

- **Endpoint:** `GET /table/faq`
- **Fields:** `id`, `question`, `answer`, `order`

#### Configuration Table

```f3
config key text; value text.
```

- **Endpoint:** `GET /table/config`
- **Fields:** `id`, `key`, `value`

## Docker Configuration

### Environment Variables

| Variable    | Description                     | Required |
| ----------- | ------------------------------- | -------- |
| `F3_CONFIG` | Complete F3 table specification | Yes      |

### Volume Mounts

| Path            | Description           |
| --------------- | --------------------- |
| `/data/db`      | LMDB database files   |
| `/data/uploads` | Uploaded images/files |

### Example docker-compose.yml

```yaml
version: "3.8"
services:
  leancms:
    image: ivantsoninski/leancms:latest
    ports:
      - "8080:8080"  # Match the port in F3_CONFIG
    environment:
      F3_CONFIG: |
        CMSSpec tables {
            posts title text; content html; author text; published date.
            pages slug text; title text; content html.
            settings key text; value text.
        }; login "admin":"supersecret"; apiKey "your-api-key-here"; uploadsURLPath "cms-files".
        system runWebServer 8080.
    volumes:
      - ./cms-data:/data/db
      - ./cms-uploads:/data/uploads
```

### Simple Docker Run

```bash
docker run -p 8080:8080 -e F3_CONFIG='CMSSpec tables { blog title text; content html; published date. }; login "admin":"password"; apiKey "your-secret-key"; uploadsURLPath "uploads". system runWebServer 8080.' ivantsoninski/leancms:latest
```

## Integration Examples

### JavaScript

```javascript
// Fetch blog posts
const response = await fetch("/table/blog", {
  headers: {
    Accept: "application/json",
    Authorization: "Bearer your-api-key",
  },
});
const posts = await response.json();
```

## Security Notes

- Always use strong passwords and API keys in production
- The API key and login credentials are defined in the F3 configuration
- Consider using environment variables for sensitive configuration
- Use HTTPS in production environments

## Design Philosophy

LeanCMS is **intentionally simple**. These aren't limitations - they're features that keep it lean:

- **Read-only JSON API** - Write operations use the web interface. Keep it simple.
- **No relations** - Independent tables are easier to understand and modify
- **Four field types only** - Text, HTML, date, number. That's all you need.
- **No complex validation** - Basic type checking prevents obvious errors
- **Single admin user** - No user management complexity. One login, done.

**If you need more features, use a different CMS.** LeanCMS does one thing well: simple content management for simple projects.

## Need Help?

LeanCMS is designed to be self-explanatory, but if you need help:

1. Check the web interface - it shows all available tables and fields
2. Use your browser's developer tools to inspect API calls
3. Remember: if it's not obvious, it might be too complex for LeanCMS!
