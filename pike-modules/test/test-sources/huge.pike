// $Id: 7271a186e014bf048131ad69590f4bc72bc2c434 $

//! Various top-level accessor and factory functions for database objects.

#include "rep.h"


// Table access and tools

#define SQL_TABLE(NAME)							\
  SqlTools.SqlTable NAME ## _table() {					\
    SqlTools.SqlTable tbl = REP.get_table (#NAME);			\
    ASSERT_IF_DEBUG (tbl);						\
    return tbl;								\
  }
#define CACHED_TABLE(NAME)						\
  REP.CachedTable NAME ## _table() {					\
    REP.CachedTable tbl = REP.get_table (#NAME);			\
    ASSERT_IF_DEBUG (tbl);						\
    return tbl;								\
  }
#define OBJECT_TABLE(NAME)						\
  REP.ObjectTable NAME ## _table() {					\
    REP.ObjectTable tbl = REP.get_table (#NAME);			\
    ASSERT_IF_DEBUG (tbl);						\
    return tbl;								\
  }

//! @ignore
// This defines *_table() functions to get the table object of the
// appropriate type for every table in REP.
// counters left out since it's only used internally by REP.SqlCounter.
SQL_TABLE       (assignments);
OBJECT_TABLE	(editions);
SQL_TABLE	(exports);
SQL_TABLE	(external_items);
SQL_TABLE	(feed_categories);
SQL_TABLE	(feed_files);
SQL_TABLE	(feed_items);
SQL_TABLE	(file_types);
SQL_TABLE	(page_categories);
CACHED_TABLE	(page_groups);
OBJECT_TABLE	(page_slots);
SQL_TABLE	(page_status);
SQL_TABLE	(page_types);
OBJECT_TABLE	(page_versions);
CACHED_TABLE	(pages);
OBJECT_TABLE	(publications);
CACHED_TABLE	(story_app_parts);
SQL_TABLE	(story_apps);
OBJECT_TABLE	(story_categories);
OBJECT_TABLE    (ads);

//  LDE
CACHED_TABLE	(labels);
OBJECT_TABLE	(styles);
SQL_TABLE	(constraints);
SQL_TABLE	(parameters);
OBJECT_TABLE	(field_defs);
OBJECT_TABLE	(component_defs);
OBJECT_TABLE    (component_area_defs);
OBJECT_TABLE	(story_tmpl_defs);
CACHED_TABLE    (lde_defs);
OBJECT_TABLE	(user_property_defs);

OBJECT_TABLE	(stories);
CACHED_TABLE	(story_items);
OBJECT_TABLE	(story_item_versions);
SQL_TABLE	(story_item_status);

SQL_TABLE       (data_field_placements);

SQL_TABLE       (external_ids);

SQL_TABLE (changelist);
SQL_TABLE (changelist_consumers);

OBJECT_TABLE    (buckets);
OBJECT_TABLE    (bucket_items);
//! @endignore

#undef SQL_TABLE
#undef CACHED_TABLE
#undef OBJECT_TABLE

//! Constants for table lock types.
constant READ_LOCK = 1;
constant WRITE_LOCK = 2;

class TableLock
//! Helper class that locks some tables on creation and unlock them on
//! destruction, to ensure table locks are unlocked even if the
//! function throws.
//!
//! Recursive locking is allowed, but nested locks won't have any
//! effect except that there are debug checks that verifies that the
//! nested lock is a subset of the outermost one.
{
  REP.PrintDBModule.ThreadBoundSqlConn tb_conn;

  protected REP.REPSession ses;
  protected array(REP.RWMutex.WriterKey) table_locks; // 0 in nested locks.

  protected void create (mapping(string:int(1..2)) tables)
  //! Takes a mapping of the tables to lock. Indices are table names
  //! and values are @[READ_LOCK] or @[WRITE_LOCK].
  {
    ses = REP.get_session();

    if (ses->locked_tables) {
#ifdef DEBUG
      // Check that the nested lock is a subset of the outer one.
      foreach (tables; string table; int lock) {
        ASSERT_IF_DEBUG (lock == READ_LOCK || lock == WRITE_LOCK);
        ASSERT_IF_DEBUG (ses->locked_tables[table /*%O*/] /*%O*/ >= lock /*%O*/,
                         table, ses->locked_tables[table], lock);
      }
#endif

#ifdef DEBUG_TABLELOCK
      werror ("Got nested %O\n", this);
#endif
    }

    else {
      ASSERT_IF_DEBUG (!page_slot_order_is_locked());

      ses->locked_tables = tables + ([]);

      // First lock the internal table locks in a well defined order.
      table_locks = ({});
      foreach (sort (indices (tables)), string table)
        if (REP.DBTable tbl = ses->db_tables[table])
          if (REP.RWMutex table_lock = tbl->table_lock)
            table_locks += ({table_lock->write_lock()});

      // Next lock in mysql. Need a thread bound connection since the
      // locks are connection bound.
      tb_conn = ses->print_db->ThreadBoundSqlConn();
      array(string) sql_locks = ({});
      foreach (tables; string table; int lock) {
        ASSERT_IF_DEBUG (lock == READ_LOCK || lock == WRITE_LOCK);
        sql_locks += ({"`" + table + "` " +
                       (lock == WRITE_LOCK ? "WRITE" : "READ")});
      }
      // Note that mysql silently replaces the table locks if a LOCK
      // TABLES already is in effect. It doesn't complain if UNLOCK
      // TABLES is used without a lock either. That's one reason we
      // have to be careful with recursive locks.
      tb_conn->conn->big_query ("LOCK TABLES " + (sql_locks * ","));

#ifdef DEBUG_TABLELOCK
      werror ("Got %O\n", this);
#endif
    }
  }

  protected void destroy()
  {
#ifdef DEBUG_TABLELOCK
    werror ("Releasing %O\n", this);
#endif

    ASSERT_IF_DEBUG (ses == REP.get_session());
    ASSERT_IF_DEBUG (ses->locked_tables);

    if (table_locks) {
      ses->locked_tables = 0;

      // Probably not necessary, but destruct the locks explicitly
      // just to avoid doubt.
      foreach (table_locks, REP.RWMutex.WriterKey lock)
        destruct (lock);

      if (tb_conn->conn)
        // If the connection is destructed then the locks are gone anyway.
        tb_conn->conn->big_query ("UNLOCK TABLES");
    }
  }

  //! @ignore
  DECLARE_OBJ_COUNT;
  //! @endignore

  protected string _sprintf (int flag)
  {
    if (flag != 'O') return 0;
    if (table_locks)
      return sprintf ("TableLock(%O, {%s})" + OBJ_COUNT,
                      tb_conn && tb_conn->thread,
                      ses && ses->locked_tables ?
                      map ((array) ses->locked_tables,
                           lambda (array(string|int) ent) {
                             return ent[0] + ":" +
                               (ent[1] == WRITE_LOCK ? "w" : "r");
                           }) * "," : "0");
    else
      return sprintf ("TableLock(%O, nested)" + OBJ_COUNT,
                      tb_conn && tb_conn->thread);
  }
}

int got_table_lock (mapping(string:int(1..2)) tables, void|REP.REPSession ses)
//! Takes a set of table locks like @[TableLock] does, but only checks
//! that at least those locks are currently taken, and returns nonzero
//! in that case.
{
  ASSERT_IF_DEBUG (sizeof (tables));
  if (!ses) ses = REP.get_session();
  if (!ses->locked_tables) return 0;
  foreach (tables; string table; int lock) {
    ASSERT_IF_DEBUG (lock == READ_LOCK || lock == WRITE_LOCK);
    if (ses->locked_tables[table] < lock)
      return 0;
  }
  return 1;
}

mapping merge_lock_policies(mixed ... /* mapping, ..., mapping */ lock_mappings)
//! Takes two or more lock mappings and produces a union where each lock
//! is set to the maximum necessary type (i.e. READ + WRITE => WRITE).
{
  //  The use of max() below depends on WRITE_LOCK > READ_LOCK > 0.
  ASSERT_IF_DEBUG((WRITE_LOCK > READ_LOCK) && (READ_LOCK > 0));

  mapping res = ([ ]);
  foreach (lock_mappings, mapping lm) {
    foreach (lm; string lm_key; int lm_type)
      res[lm_key] = max(res[lm_key], lm_type);
  }
  return res;
}

class DBMaintenanceLock
//! This lock allows a section of code to do batch updates on records
//! without regard to the caches in @[REP.CachedTable] and
//! @[REP.ObjectTable] objects. When the lock is released, the caches
//! in all such tables are zapped.
//!
//! The notification queue is process synchronously in the current
//! thread when the lock is acquired. It is also processed again when
//! the lock is released, just before the object caches are zapped.
//! Notifications are not processed in the background during the
//! lifetime of the lock.
//!
//! As a design principle, it can be assumed that no other REP session
//! (i.e. @[REP.REPSession] object) is active during the lifetime of
//! this lock.
//!
//! @note
//! This class does not lock the tables in MySQL, although that would
//! probably be a good idea. The reason it's left to the caller is
//! that all tables must be locked at once, and we don't know what
//! other tables the caller needs to lock.
//!
//! @fixme
//! This lock currently affects all @[REP.CachedTable] and
//! @[REP.ObjectTable] objects, something that might need to be
//! refined later. The reason for the current blunt approach is to not
//! have to bother with cached references between @[REP.DBObject]s in
//! different tables.
//!
//! @fixme
//! There ought to be a notification when the lock is released so that
//! it's possible for subscribers to recover.
{
  protected REP.PrintDBModule.NotificationQueue.SyncProcessNotifications
    sync_proc;
  protected int finished;

  protected void create()
  {
    REP.REPSession session = REP.get_session();

    mixed err = catch {
        // Time out after an hour if we can't get an exclusive lock.
        session->make_exclusive(3600);
      };
    if (err) {
      // Probably timeout.
      // Make us finished to keep the assert in destroy() happy.
      finished = 1;
      throw(err);
    }

    REP.PrintDBModule print_db = REP.get_print_module();
    REP.PrintDBModule.NotificationQueue queue = print_db->notification_queue;

    sync_proc = queue->SyncProcessNotifications();

    int count = 100;
    do {
      sync_proc->process();

      // This is necessary to move the notification calls to the
      // notification queue so that they all get processed. Also,
      // considering the nature of this lock, it can be argued that the
      // REP session ends here in many aspects.
      REP.get_session()->execute_session_done_callbacks();
    } while (queue->size() && count--); // Repeat until the queue is
                                        // empty to make sure we
                                        // process all cascade
                                        // notifications triggered by
                                        // session done callbacks.

    if (!count)
      error ("Could not flush notification queue.");

    // Currently only a single lock is allowed. Can be relaxed if it
    // turns out to be necessary.
    ASSERT_IF_DEBUG (!print_db->has_db_maintenance_lock++);
  }

  //! Finish up - should be called right before the object is about to
  //! be destructed. The reason we can't do this in destroy() is that
  //! we don't want to do things requiring mutex locks there due to
  //! the risk of being called while those mutex locks are already
  //! taken. C.f. [Bug 6892].
  this_program finish()
  {
    REP.PrintDBModule print_db = REP.get_print_module();
    REP.PrintDBModule.NotificationQueue queue = print_db->notification_queue;

    int count = 100;
    do {
      sync_proc->process();

      // This is necessary to move the notification calls to the
      // notification queue so that they all get processed. Also,
      // considering the nature of this lock, it can be argued that the
      // REP session ends here in many aspects.
      REP.get_session()->execute_session_done_callbacks();
    } while (queue->size() && count--); // Repeat until the queue is
                                        // empty to make sure we
                                        // process all cascade
                                        // notifications triggered by
                                        // session done callbacks.

#if defined(RUN_SELF_TEST) && defined(REP_ZAP_GARBAGE)
    // Attempt to increase the likelyhood of triggering a race-condition.
    sleep(1);
#endif

    print_db->zap_dbobject_caches();
    destruct (sync_proc);

#if defined(RUN_SELF_TEST) && defined(REP_ZAP_GARBAGE)
    // Attempt to increase the likelyhood of triggering a race-condition.
    sleep(1);

    roxen.describe_all_threads(UNDEFINED, 1);
#endif

    REP.REPSession session = REP.get_session();
    session->make_nonexclusive();

    // Reset client sessions since client-side data may be obsolete
    // after GC/cache flushes.
    foreach (print_db->client_sessions;
             string session_id;
             REP.ClientSession cs) {
      cs->reset_session (1);
    }

    ASSERT_IF_DEBUG (!--print_db->has_db_maintenance_lock);
    finished = 1;
    return this;
  }

  protected void destroy()
  {
    // finish() must be called before the object is destructed.
    ASSERT_IF_DEBUG (finished);
  }

  void process_notification_queue()
  //! Process any queued notifications (synchronously).
  {
    sync_proc->process();
  }

  //! @ignore
  DECLARE_OBJ_COUNT;
  //! @endignore

  protected string _sprintf (int flag)
  {
    return flag == 'O' && ("DBMaintenanceLock()" + OBJ_COUNT);
  }
}

// Mapping from id to table name.
// THESE MUST NEVER CHANGE (or existing databases will become inconsistent).
// Appending is fine, though.
constant object_tables_by_id = ([
  1: "publications",
  2: "editions",
  3: "page_groups",
  4: "page_versions",
  5: "pages",
  6: "page_slots",
  7: "stories",
  8: "story_items",
  9: "story_item_versions",
  10: "buckets",
  11: "ads",
  12: "bucket_items",
  13: "story_tmpl_defs",
  14: "component_defs",
  15: "component_area_defs",
  16: "field_defs",
  17: "styles",
  18: "story_categories",
  19: "user_property_defs",
  20: "labels",
  21: "lde_defs",
  22: "story_app_parts",
]);

constant object_table_ids_by_table = ([
  "publications":        1,
  "editions":            2,
  "page_groups":         3,
  "page_versions":       4,
  "pages":               5,
  "page_slots":          6,
  "stories":             7,
  "story_items":         8,
  "story_item_versions": 9,
  "buckets":             10,
  "ads":                 11,
  "bucket_items":        12,
  "story_tmpl_defs":     13,
  "component_defs":      14,
  "component_area_defs": 15,
  "field_defs":          16,
  "styles":              17,
  "story_categories":    18,
  "user_property_defs":  19,
  "labels":              20,
  "lde_defs":            21,
  "story_app_parts":     22,
]);


//! Returns the DBObject identified by the parameters
//! @[object_table_id] and @[object_id].
//!
//! @seealso
//! @[table_and_object_id_by_dbobj]
mapping(string:mixed)|REP.DBObject
dbobj_by_table_and_id (int object_table_id, int object_id,
                       void|REP.OnError on_not_found)
{
  string table_name = object_tables_by_id[object_table_id];
  ASSERT_IF_DEBUG (table_name);

  REP.DBTable tbl = REP.get_table (table_name);
  ASSERT_IF_DEBUG (tbl);

  function(int,REP.OnError:REP.DBObject) lookup;

  if (tbl->object_get) {
    lookup = tbl->object_get;
  } else if (tbl->cached_get) {
    if (table_name == "pages") {
      lookup = current_page_by_id;
    } else {
      lookup = tbl->cached_get;
    }
  }

  ASSERT_IF_DEBUG (lookup);
  return lookup (object_id, on_not_found);
}

//! Returns an array ({ object_table_id, object_id }) identifying the
//! object @[dbo]. It can also handle some CachedTable-style mapping
//! records.
//!
//! @seealso
//! @[dbobj_by_table_and_id]
array(int) table_and_object_id_by_dbobj (mapping(string:mixed)|REP.DBObject dbo)
{
  string table_name;
  int object_id;

  if (objectp (dbo)) {
    object_id = dbo->id();
    if (dbo->is_rep_db_object) {
      table_name = dbo->table_name;
    } else if (dbo->is_rep_page) {
      table_name = "pages";
    }
  } else if (mappingp (dbo)) {
    int id;
    if (id = dbo->story_item_id) {
      table_name = "story_items";
      object_id = id;
    } else if (id = dbo->page_group_id) {
      table_name = "page_groups";
      object_id = id;
    }
  }

  ASSERT_IF_DEBUG (table_name);
  int object_table_id = object_table_ids_by_table[table_name];
  ASSERT_IF_DEBUG (object_table_id);
  ASSERT_IF_DEBUG (object_id);

  // Extra paranoia check here: returning bogus data can have
  // seriously bad effects on the database (that aren't revealed until
  // the next lookup).
  if (!object_table_id || !object_id) {
    error ("Couldn't lookup %O (%O %O).", dbo, object_table_id, object_id);
  }

  return ({ object_table_id, object_id });
}

mapping(string:mixed)|REP.DBObject
resolve_unique_object_id (string uniq_obj_id,
                          void|REP.OnError on_not_found)
//! Resolve a string returned by @[unique_object_id_by_dbobj].
{
  array(string) segs = uniq_obj_id / ":";
  string table_name = segs[0];
  int object_table_id = object_table_ids_by_table[table_name];
  if (!object_table_id) error ("Object table %O not found.\n", table_name);
  int object_id = (int)segs[1];
  return dbobj_by_table_and_id (object_table_id, object_id, on_not_found);
}

REP.DBObject.ExtFieldRef ext_field_ref_from_path (string path,
                                                  void|REP.OnError on_not_found)
{
  array(string) segments = path / "/";
  string unique_object_id = segments[0..1] * ":";
  string ext_field = segments[2];
  int|string subspec;
  if (sizeof (segments) >= 5) {
    if (sscanf (segments[3], "i-%d", int idx)) {
      subspec = idx;
    } else if (sscanf (segments[3], "s-%s", string idx)) {
      subspec = idx;
    } else {
      return REP.raise_err (on_not_found, "Invalid subspec %s.\n",
                            segments[3]);
    }
  }

  string filename;
  string extension;

  if (subspec) {
    if (sizeof (segments) >= 5)
      filename = segments[-1];
  } else if (sizeof (segments) >= 4) {
    filename = segments[-1];
  }

  extension = filename && has_value (filename, ".") && (filename / ".")[1];

  if (REP.DBObject dbobj = resolve_unique_object_id (unique_object_id,
                                                     on_not_found)) {
    if (dbobj->avail_ext_fields[ext_field]) {
      array(REP.DBObject.ExtFieldRef) matching_refs =
        filter (Array.flatten (({dbobj->ext_field_refs_for_field (ext_field)})),
                lambda (REP.DBObject.ExtFieldRef field_ref) {
                  return !subspec ||
                    (field_ref && field_ref->subspec == subspec);
                });
      if (sizeof (matching_refs))
        return matching_refs[0];
      return REP.raise_err (on_not_found, "Subspec %O not found.\n", subspec);
    } else {
      return REP.raise_err (on_not_found, "Invalid field %s.\n", ext_field);
    }
  }

  return REP.raise_err (on_not_found, "Couldn't resolve path %s.\n",
                        path);
}

string unique_object_id_by_dbobj (mapping(string:mixed)|REP.DBObject dbo)
//! Returns a string that uniquely identifies this object, suitable
//! for use in external components, such as the UI. The string can be
//! resolved through @[REP.DB.resolve_unique_object_id].
{
  array(int) ids = table_and_object_id_by_dbobj (dbo);
  string tbl_name = object_tables_by_id[ids[0]];
  if (!tbl_name) error ("Table id %O not found", ids[0]);
  return tbl_name + ":" + ids[1];
}

// Note: These must never change, since that would break existing
// data. Adding new values is fine, though.
mapping(int:program) `bucket_item_classes_by_id()
{
  return ([
    1: REP["BucketItem"],
    2: REP["PagePlacement"],
    3: REP["StoryPlacement"],
    4: REP["SIVPlacement"],
    5: REP["AdPlacement"],
    6: REP["AdAppearance"],
    7: REP["PackageAppearance"],
  ]);
}

mapping(program:int) `bucket_item_ids_by_class()
{
  mapping(int:program) m = bucket_item_classes_by_id;
  return mkmapping (values (m), indices (m));
}

object/*REP.BucketItem*/ bucket_item_factory (REP.ObjectTable tbl, int new_rec,
                                              mapping(string:mixed) rec)
// Registered as DBObject factory callback in bucket_items_table().
{
  program obj_prog = bucket_item_classes_by_id[rec->bucket_item_type_id];
  if (!obj_prog) {
    ASSERT_IF_DEBUG (bucket_item_classes_by_id[rec->bucket_item_type_id /*%O*/],
                     rec->bucket_item_type_id);
    obj_prog = REP["BucketItem"];
  }
  return obj_prog (!new_rec && rec, tbl);
}

//!
REP.Bucket bucket_by_id (int(1..) bucket_id, void|REP.OnError on_not_found)
{
  return buckets_table()->object_get (bucket_id, on_not_found);
}

//!
REP.Bucket create_bucket (void|mapping(string:mixed) init_values,
                          void|REP.OnError on_value_error)
{
  return buckets_table()->object_create (init_values, on_value_error);
}

//! Returns all buckets where @[dbo] is present. If
//! @[bucket_item_type] is specified, the search is restricted to
//! BucketItems of that type (e.g. @[REP.StoryPlacement]).
array(REP.Bucket) buckets_for_dbobj (mapping(string:mixed)|REP.DBObject dbo,
                                     void|program bucket_item_type)
{
  [int object_table_id, int object_id] = table_and_object_id_by_dbobj (dbo);
  array(string) where_parts = ({ "object_table_id = :object_table_id",
                                 "object_id = :object_id",
                                 "delete_at = 0" });
  mapping(string:mixed) bindings = ([ "object_table_id": object_table_id,
                                      "object_id": object_id ]);

  if (bucket_item_type) {
    int bucket_item_type_id = bucket_item_ids_by_class[bucket_item_type];
    if (!bucket_item_type_id)
      error ("Invalid bucket type %O.\n", bucket_item_type);
    where_parts += ({ "bucket_item_type_id = :bucket_item_type_id" });
    bindings["bucket_item_type_id"] = bucket_item_type_id;
  }

  array(int) bucket_ids =
    Array.uniq (bucket_items_table()->select1 ("bucket_id",
                                               ({ where_parts * " AND ",
                                                  bindings })));
  return buckets_table()->object_get_multi (bucket_ids);
}

//! Returns all @[REP.BucketItem]:s from which @[dbo] is
//! referenced. if @[bucket] is specified, only items in that bucket
//! are returned.  If @[bucket_item_type] is specified, the search is
//! restricted to BucketItems of that type
//! (e.g. @[REP.StoryPlacement]).
//!
//! If @[only_current_items] is set, only items that return 1 from
//! @[is_current] will be returned.
array(REP.BucketItem)
bucket_items_for_dbobj (mapping(string:mixed)|REP.DBObject dbo,
                        void|REP.Bucket bucket,
                        void|program bucket_item_type,
                        void|int(0..1) only_current_items)
{
  [int object_table_id, int object_id] = table_and_object_id_by_dbobj (dbo);
  array(string) where_parts = ({ "object_table_id = :object_table_id",
                                 "object_id = :object_id",
                                 "delete_at = 0" });
  mapping(string:mixed) bindings = ([ "object_table_id": object_table_id,
                                      "object_id": object_id ]);

  if (bucket) {
    where_parts += ({ "bucket_id = :bucket_id" });
    bindings["bucket_id"] = bucket->id();
  }

  if (bucket_item_type) {
    int bucket_item_type_id = bucket_item_ids_by_class[bucket_item_type];
    if (!bucket_item_type_id)
      error ("Invalid bucket type %O.\n", bucket_item_type);
    where_parts += ({ "bucket_item_type_id = :bucket_item_type_id" });
    bindings["bucket_item_type_id"] = bucket_item_type_id;
  }

  return filter (bucket_items_table()->object_select (({ where_parts * " AND ",
                                                         bindings }),
                                                      UNDEFINED,
                                                      "ORDER BY `bucket_id`, "
                                                      "`order`"),
                 lambda (REP.BucketItem item)
                 {
                   if (only_current_items) {
                     return item->is_current();
                   }

                   return 1;
                 });
}

// Generates a version 5 UUID (hash-based) from namespace and id, used
// to lookup records in the cache.
protected string external_uuid_cache_key (string namespace, string id)
{
  constant my_namespace = "d38ed85a-dd99-43a4-becb-4acc72da42c5";
  return Standards.UUID.make_version5 (string_to_utf8(namespace + id),
                                       my_namespace)->str();
}

//! Associates the @[REP.DBObject]/@[REP.CachedTable] rec mapping
//! @[dbo] with the external id @[id] in namespace @[namespace],
//! unless the @[namespace]/@[id] combination exists already. The
//! @[REP.DBObject]/@[REP.CachedTable] rec mapping that's actually
//! associated is returned.
REP.DBObject|mapping(string:mixed)
lookup_or_add_external_id (REP.DBObject|mapping(string:mixed) dbo,
                           string namespace,
                           string id,
                           string|void origin_id,
                           string|void origin_type)
{
  SqlTools.SqlTable uuids_table = external_ids_table();
  [int object_table_id, int object_id] = table_and_object_id_by_dbobj (dbo);

  mapping(string:mixed) insert_rec =
    ([ "namespace": namespace,
       "ext_id": id,
       "object_table_id": object_table_id,
       "object_id": object_id,
    ]);
  if (origin_id) insert_rec["origin_ext_id"] = origin_id;
  if (origin_type) insert_rec["origin_type"] = origin_type;

  REP.PrintDBModule print_db = REP.get_print_module();
  string cache_key = external_uuid_cache_key (namespace, id);

  REP.DBObject res;

  if (uuids_table->insert_ignore (insert_rec)) {
    // Insert successful (i.e. no mapping existed previously).
    print_db->clear_cached_rec_id_by_uuid (cache_key);
    res = dbo;
  } else {
    res = lookup_external_id (namespace, id, REP.RETURN_ZERO);

    if (!res) {
      // There was a mapping in external_ids but lookup_external_id
      // failed, so the external_ids mapping is stale. Replace it.
      uuids_table->replace (insert_rec);
      print_db->clear_cached_rec_id_by_uuid (cache_key);
      res = lookup_external_id (namespace, id);
    }
  }

  return res;
}

//! Resolves the @[REP.DBObject]/@[REP.CachedTable] rec mapping
//! @[dbo] from the external id @[id] in namespace @[namespace].
REP.DBObject|mapping(string:mixed)
lookup_external_id (string namespace,
                    string id,
                    void|REP.OnError on_not_found)
{
  REP.PrintDBModule print_db = REP.get_print_module();
  string cache_key = external_uuid_cache_key (namespace, id);
  int(-1..0)|array(int) object_ids;

  if (!(object_ids = print_db->cached_rec_id_by_uuid (cache_key))) {
    SqlTools.SqlTable uuids_table = external_ids_table();

    SqlTools.SqlTable.Result res =
      uuids_table->select (({ "namespace = :namespace AND "
                              "ext_id = :ext_id",
                              ([ "namespace": namespace,
                                 "ext_id": id, ]) }),
                           ({ "object_table_id",
                              "object_id" }));
    int num_rows = res->num_rows();

    if (num_rows == 1) {
      mapping(string:mixed) rec = res->fetch();
      object_ids = ({ rec->object_table_id, rec->object_id });
    } else if (!num_rows) {
      object_ids = -1;
    } else {
      // Should never happen, thanks to table constraints.
      error ("Multiple entries found for ID %s in namespace %s.\n",
             id, namespace);
    }

    print_db->set_cached_rec_id_by_uuid (cache_key, object_ids);
  }

  if (object_ids == -1)
    return REP.raise_err (on_not_found,
                          "ID %s in namespace %s not found.\n",
                          id, namespace);

  return dbobj_by_table_and_id (@object_ids, on_not_found);
}

//! Return all @[REP.DBObject]/@[REP.CachedTable] rec mappings
//! @[dbo] with the original external ID @[id] in namespace @[namespace].
array(REP.DBObject) lookup_origin_external_id (string namespace, string id)
{
  return lookup_origin_external_ids(namespace, ({ id }) );
}

//! Return all @[REP.DBObject]/@[REP.CachedTable] rec mappings
//! @[dbo] with any of the original external IDs @[ids] in namespace
//! @[namespace].
array(REP.DBObject) lookup_origin_external_ids (string namespace,
                                                array(string) ids)
{
  if (!sizeof(ids)) return ({});

  REP.PrintDBModule print_db = REP.get_print_module();
  SqlTools.SqlTable uuids_table = external_ids_table();

  SqlTools.SqlTable.Result sql_res =
    uuids_table->select (({ "namespace = :namespace AND "
                            "origin_ext_id IN ('" + (ids * "','") + "')",
                            ([ "namespace": namespace ]) }),
                         ({ "object_table_id",
                            "object_id" }));

  array(REP.DBObject) res = ({});
  while (mapping(string:mixed) rec = sql_res->fetch()) {
    array(int) object_ids = ({ rec->object_table_id, rec->object_id });
    res += ({ dbobj_by_table_and_id (@object_ids, REP.RETURN_ZERO) });
  }

  return res - ({ 0 });
}

//! Removes the external reference given @[namespace] and @[id].
void remove_external_id(string namespace, string id)
{
  REP.PrintDBModule print_db = REP.get_print_module();
  string cache_key = external_uuid_cache_key(namespace, id);
  print_db->clear_cached_rec_id_by_uuid(cache_key);

  SqlTools.SqlTable uuids_table = external_ids_table();
  uuids_table->delete( ({ "namespace = :namespace AND ext_id = :ext_id",
                          ([ "namespace" : namespace,
                             "ext_id"    : id ]) }) );
}


//! Returns all external IDs associated with the
//! @[REP.DBObject]/@[REP.CachedTable] rec mapping in namespace
//! @[namespace].
array(string) external_ids_for_dbobj (REP.DBObject|mapping(string:mixed) dbo,
                                      string namespace)
{
  SqlTools.SqlTable uuids_table = external_ids_table();

  [int object_table_id, int object_id] = table_and_object_id_by_dbobj (dbo);

  return uuids_table->select1 ("ext_id",
                               ({ "namespace = :namespace AND "
                                  "object_table_id = :object_table_id AND "
                                  "object_id = :object_id",
                                  ([ ":namespace": namespace,
                                     ":object_table_id": object_table_id,
                                     ":object_id": object_id, ]) }));
}

//! Returns all origin external IDs associated with the
//! @[REP.DBObject]/@[REP.CachedTable] rec mapping in namespace
//! @[namespace].
array(string) origin_external_ids_for_dbobj (REP.DBObject|mapping(string:mixed) dbo,
                                             string namespace)
{
  SqlTools.SqlTable uuids_table = external_ids_table();

  [int object_table_id, int object_id] = table_and_object_id_by_dbobj (dbo);

  return uuids_table->select1 ("origin_ext_id",
                               ({ "namespace = :namespace AND "
                                  "object_table_id = :object_table_id AND "
                                  "object_id = :object_id",
                                  ([ ":namespace": namespace,
                                     ":object_table_id": object_table_id,
                                     ":object_id": object_id, ]) }));
}

//! Returns all namespace and origin types associated with the
//! @[REP.DBObject]/@[REP.CachedTable] rec mapping in @[dbo].
array(array(string))
  namespaces_and_origin_types_for_dbobj(REP.DBObject|mapping(string:mixed) dbo)
{
  SqlTools.SqlTable uuids_table = external_ids_table();

  [int object_table_id, int object_id] = table_and_object_id_by_dbobj (dbo);

  SqlTools.SqlTable.Result sql_res =
    uuids_table->select( ({ "object_table_id = :object_table_id AND "
                            "object_id = :object_id",
                            ([ ":object_table_id": object_table_id,
                               ":object_id": object_id ]) }),
                         ({ "namespace", "origin_type" }) );

  array(array(string)) res = ({ });
  while (mapping(string:mixed) rec = sql_res->fetch()) {
    array(string) entry = ({ rec->namespace, rec->origin_type });
    res += ({ entry });
  }
  return res;
}

//! Normalizes metadata for externally stored data values. Used by the
//! DBObject class and the REP GC module. The reason this function
//! resides here is for the GC to be able to use it without
//! instantiating a DBObject.
REP.DBObject.ext_val_info normalize_dbobject_val_info (
  REP.DBObject.ext_val_info|string val_info)
{
  if (val_info) {
    if (stringp (val_info) && sizeof (val_info) >= 5 &&
        has_prefix (val_info, "�ke")) {
      val_info = decode_value (val_info);
    }
    if (intp (val_info)) {
      // Backwards compat.
      val_info =
        ([ REP.DBObject.STORAGE_COUNTER: val_info,
           REP.DBObject.STORAGE_TYPE: REP.DBObject.TYPE_BINARY_STRING ]);
    }
    if (mappingp (val_info) && sizeof (val_info) == 2 &&
        val_info[REP.DBObject.STORAGE_TYPE] &&
        val_info[REP.DBObject.STORAGE_COUNTER])
      return val_info;
  }
  return UNDEFINED;
}

REP.Ad ad_by_id (int ad_id, void|REP.OnError on_rec_not_found)
{
  REP.ObjectTable tbl = ads_table();
  return tbl->object_get(ad_id, on_rec_not_found);
}

REP.Ad ad_by_uuid (string ad_uuid, void|REP.OnError on_rec_not_found)
{
  REP.ObjectTable tbl = ads_table();
  if (REP.Ad ad = ad_by_id (rec_id_by_uuid (tbl, ad_uuid), REP.RETURN_ZERO)) {
    return ad;
  }

  return REP.raise_err(on_rec_not_found,
                       "Record uuid '%s' not found in %s.\n",
                       ad_uuid, tbl->table);
}

// Page group versions

REP.PGVersion pgv_by_id (int(1..) page_version_id,
                         void|REP.OnError on_rec_not_found)
{
  return page_versions_table()->object_get (page_version_id, on_rec_not_found);
}

array(REP.PGVersion) pgvs_by_storage_counter (int(1..) storage_counter)
{
  return page_versions_table()->object_select (sprintf ("storage_counter = %d",
                                                        storage_counter));
}

array(REP.PGVersion) current_pgvs_by_edition(REP.Edition ed,
                                             void|REP.Edition.Section sec)
{
  ASSERT_IF_DEBUG(ed && ed->id());
  ASSERT_IF_DEBUG(!sec || (sec->edition() == ed));

  REP.ObjectTable pgv_tbl = page_versions_table();
  array(REP.PGVersion) ret =
    pgv_tbl->object_select("page_groups.edition_id = " + (string)ed->id(),
                           "INNER JOIN page_groups "
                           "ON page_versions.page_version_id = "
                           "   page_groups.cur_pgversion_id");

  //  No simple way to filter for a given edition without grabbing all PGVs
  //  above and then check first slot per PGV.
  if (sec) {
    int sec_id = sec->id();
    ret = filter(ret, lambda(REP.PGVersion pgv) {
        if (REP.PageSlot first_ps = pgv->first_page_slot())
          if (first_ps->get("section_id") == sec_id)
            return true;
        return false;
      });
  }

  return ret - ({ 0 });
}

REP.PGVersion current_pgv_by_pg_id (int page_group_id,
                                    void|REP.OnError on_rec_not_found)
//! Returns zero regardless of @[on_rec_not_found] if the page group
//! has no current page version.
{
  ASSERT_IF_DEBUG(intp(page_group_id));
  mapping(string:mixed) pg =
    page_groups_table()->cached_get (page_group_id, on_rec_not_found);
  if (!pg) return 0;

  int cur_pgversion_id = pg->cur_pgversion_id;
  return cur_pgversion_id &&
    page_versions_table()->object_get (cur_pgversion_id, on_rec_not_found);
}

protected int rec_id_by_uuid (REP.DBTable tbl, string uuid)
// Get record id by record UUID in the table @[tbl]. It is assumed
// that if the record id's column name is "foobar_id", the uuid column
// name is "foobar_uuid".
{
  REP.PrintDBModule print_db = REP.get_print_module();
  if (int rec_id = print_db->cached_rec_id_by_uuid (uuid)) {
    return rec_id;
  }

  string id_col = tbl->rec_id_col;
  string uuid_col = id_col[..<2] + "uuid";
  array(int) res = tbl->select1 (tbl->quote (id_col),
                                 tbl->quote (uuid_col) + " = '" +
                                 tbl->quote (uuid) + "'");
  ASSERT_IF_DEBUG (sizeof (res) <= 1);

  if (sizeof (res))
    return print_db->set_cached_rec_id_by_uuid (uuid, res[0]);

  // No negative caching at this point since that would require
  // invalidation if a record with a previously nonexisting uuid is
  // inserted.
  return 0;
}

mapping(string:mixed) pg_rec_by_uuid (string page_group_uuid,
                                      void|REP.OnError on_rec_not_found)
//! Returns the cached record mapping for the page group with the
//! given uuid. Don't be destructive on the returned mapping.
{
  REP.CachedTable pg_tbl = page_groups_table();

  int pg_id = rec_id_by_uuid (pg_tbl, page_group_uuid);

  if (!pg_id)
    return REP.raise_err (on_rec_not_found,
                          "Page group with uuid %s not found.\n",
                          page_group_uuid);

  return pg_tbl->cached_get (pg_id);
}

REP.PGVersion current_pgv_by_pg_uuid (string page_group_uuid,
                                      void|REP.OnError on_rec_not_found)
//! Returns zero regardless of @[on_rec_not_found] if the page group
//! has no current page version.
{
  mapping(string:mixed) pg_rec = pg_rec_by_uuid (page_group_uuid,
                                                 on_rec_not_found);
  if (!pg_rec) return 0;

  return current_pgv_by_pg_id (pg_rec->page_group_id, on_rec_not_found);
}

array(REP.PGVersion) pgvs_by_pg_id (int page_group_id,
                                    void|int limit,
                                    void|int include_deleted)
{
  REP.PGVersion pgv = current_pgv_by_pg_id (page_group_id, REP.RETURN_ZERO);

  if (!pgv) {
    // No current pgv for the specified page group. We'll fetch one
    // from the DB (this is a pretty rare case).
    REP.ObjectTable pv_tbl = page_versions_table();
    array(REP.PGVersion) pgvs =
      pv_tbl->object_select ("page_group_id = " + page_group_id,
                             0,
                             "LIMIT 1");
    if (sizeof (pgvs))
      pgv = pgvs[0];
    else
      return ({});
  }

  return pgv->all_versions (limit, include_deleted);
}

array(REP.PGVersion) pgvs_by_pg_uuid (string pg_uuid,
                                      void|int limit,
                                      void|int include_deleted)
//! Returns an array of all pgvs for the given page group uuid,
//! ordered descending by version.
{
  REP.ObjectTable pgv_tbl = page_versions_table();
  return pgv_tbl->object_select (
    "page_group_id IN ("
    "  SELECT page_group_id FROM page_groups"
    "  WHERE page_group_uuid='" + pgv_tbl->quote (pg_uuid) + "')" +
    (include_deleted ? "" : " AND is_deleted=0"),
    0,
    "ORDER BY page_version_id DESC" +
    (limit > 0 ? " LIMIT " + limit : ""));
}

mapping(string:array(REP.PGVersion))
multi_pgvs_by_pg_uuids (array(string) pg_uuids,
                        void|int include_deleted)
//! Returns a mapping from page group uuid to an array of
//! @[REP.PGVersion] objects for that page group. PGVersions are _not_
//! sorted in the array, for performance reasons.
{
  REP.ObjectTable pgv_tbl = page_versions_table();

  mapping(string:array(REP.PGVersion)) res = ([]);

  foreach (pg_uuids / 500.0, array(string) pg_uuid_chunk) {
    array(REP.PGVersion) pgvs =
      pgv_tbl->object_select (
        "  page_group_uuid IN (" +
        (map (pg_uuid_chunk,
              lambda (string pg_uuid)
              {
                return "'" + pgv_tbl->quote (pg_uuid) + "'";
              }) * ",") +
        ")" +
        (include_deleted ? "" : " AND page_versions.is_deleted=0"),
        "INNER JOIN page_groups ON (page_versions.page_group_id = "
        "                           page_groups.page_group_id)");
    foreach (pgvs, REP.PGVersion pgv) {
      string pg_uuid = pgv->get_unver ("page_group_uuid");
      if (!res[pg_uuid]) res[pg_uuid] = ({});
      res[pg_uuid] += ({ pgv });
    }
  }

  return res;
}

array(REP.PGVersion) sort_pgvs_by_page_slot_order (array(REP.PGVersion) pgvs)
//! Returns an array that contains the given pages sorted by the order
//! of the page slots bound to the first page in each page group.
//! Assumes that all page groups has at least one page bound to a page
//! slot. If the pages belong to different editions then the order
//! between the editions is stable based on publication title, edition date
//! and id.
//!
//! @seealso
//! @[sort_pages_by_page_slot_order]
{
  //  Separate by edition and then use comparison function inside PageSlot
  //  which will consider page_order and subspec properties. The editions
  //  are referenced using a key on this form:
  //
  //    <pub-title> : <ed-date> : <ed-id>
  //
  //  This ensures a good user experience in various interfaces while still
  //  being reasonable stable if multiple editions are targeted for the same
  //  date.
  mapping(string:array(REP.PGVersion)) sort_items = ([ ]);
  foreach (pgvs, REP.PGVersion pgv) {
    REP.Edition ed = pgv->edition();
    string ed_key = sprintf("%s\0%s\0%09d",
                            ed->publication()->get("title"),
                            (string) ed->get("publ_date"),
                            ed->id());
    if (!sort_items[ed_key])
      sort_items[ed_key] = ({ });
    sort_items[ed_key] += ({ pgv });
  }
  foreach (sort_items; string ed_key; array(REP.PGVersion) ed_pgvs) {
    array(REP.PageSlot|int(0..0)) slots = allocate(sizeof(ed_pgvs));
    foreach (ed_pgvs; int idx; REP.PGVersion ed_pgv) {
    page_loop:
      foreach (ed_pgv->pages(), REP.Page page) {
        if (REP.PageSlot slot = page->page_slot()) {
          slots[idx] = slot;
          break page_loop;
        }
      }
    }

    //  NOTE: This sorts slots from mixed sections in section order
    sort(slots, ed_pgvs);
    sort_items[ed_key] = ed_pgvs;
  }

  //  Finally traverse the edition keys in alphabetical order and collect
  //  results.
  array(REP.PGVersion) res = ({ });
  foreach (sort(indices(sort_items)), string ed_key)
    res += sort_items[ed_key];
  return res;
}

array(REP.Page) pages_by_page_slot_id (int(1..) page_slot_id)
{
  Sql.sql_result sql_res =
    REP.get_db()->big_query (
      "SELECT page_groups.cur_pgversion_id, pages.page_id "
      "  FROM pages "
      "  JOIN page_groups USING (page_group_id) "
      " WHERE pages.page_slot_id = " + page_slot_id + " "
      "   AND page_groups.cur_pgversion_id IS NOT NULL");

  array(REP.Page) res = ({});
  while (array(string) entry = sql_res->fetch_row())
    if (REP.PGVersion pgv = pgv_by_id ((int) entry[0])) {
      // Might fail to find the pgv only if there's a race with the gc.

      // Return 0 on page not found because this code is race sensitive. There
      // is a risk that the PGV no longer has the page. [EP-1595]
      if (REP.Page page = pgv->page_by_id ((int) entry[1], REP.RETURN_ZERO)) {
        ASSERT_IF_DEBUG (page /* pgv: %O, page_id: %O */, pgv, entry[1]);
        res += ({page});
      }
    }

  return res;
}

array(array(REP.Page)) multi_pages_by_page_slot_ids (
  array(int(1..)) page_slot_ids)
//! Equivalent to @expr{map (@[page_slot_ids], @[pages_by_page_slot_id])@}
//! but more efficient.
{
  if (!sizeof (page_slot_ids)) return ({});

  array(string) id_strs = (array(string)) page_slot_ids;
  mapping(string:int) id_pos = mkmapping (id_strs, indices (id_strs));

  Sql.sql_result sql_res =
    REP.get_db()->big_query (
      "SELECT page_groups.cur_pgversion_id, pages.page_id, "
      "       pages.page_slot_id "
      "  FROM pages "
      "  JOIN page_groups USING (page_group_id) "
      " WHERE pages.page_slot_id IN (" + (id_strs * ",") + ") "
      "   AND page_groups.cur_pgversion_id IS NOT NULL");

  array(array(REP.Page)) res = allocate (sizeof (page_slot_ids), ({}));
  while (array(string) entry = sql_res->fetch_row())
    if (REP.PGVersion pgv = pgv_by_id ((int) entry[0])) {
      // Might fail to find the pgv only if there's a race with the gc.
      REP.Page page = pgv->page_by_id ((int) entry[1]);
      ASSERT_IF_DEBUG (page /* pgv: %O, page_id: %O */, pgv, entry[1]);
      ASSERT_IF_DEBUG (!zero_type (id_pos[entry[2]]) /* ps_id: %O */, entry[2]);
      res[id_pos[entry[2]]] += ({page});
    }

  return res;
}

protected REP.PGVersion low_make_infant_pgv (REP.PGVersion src_pgv,
                                             ReuseStoragePolicy reuse_storage,
                                             void|REP.Edition edition)
//! Prepares an object for a new page version based on an old one. The
//! object is not inserted into the table.
//!
//! @param src_pgv
//! Populate the fields in the new object with the values from this
//! one. May be zero.
//!
//! @param reuse_storage
//! See @[add_pgv].
{
  ASSERT_IF_DEBUG (src_pgv || !reuse_storage);

  REP.PGVersion infant = page_versions_table()->create_infant();
  DO_IF_DEBUG (infant->is_live = 0);

  if (src_pgv) {
    ASSERT_IF_DEBUG (src_pgv->get_unver ("page_group_id"));
    infant->pg_volatile = src_pgv->pg_volatile;

    // Make a copy of src_pgv, but exclude fields that don't really
    // have something to do with the previous
    // version. PGVersion.set_fields will fill in appropriate defaults
    // for them. Fields related to the stored LAYOUT_FILE will be
    // updated by new_version_fixup (called below).
    infant->verbatim_copy (src_pgv,
                           ([ "user_id": 1,
                              "user_handle": 1,
                              "user_fullname": 1,
                              "date_created": 1,
                              "page_type_id": 1,
                              "comment": 1, ]));

    mapping(string:mixed) ver_fields = ([]);

    if (reuse_storage <= 0) {
      if (reuse_storage == OVERWRITEABLE_STORAGE) {
        // If src_pgv has a user_storage_counter already, we'll reuse
        // that to allow multiple overwriteable versions since the
        // last user change.
        ver_fields->user_storage_counter =
          src_pgv->get ("user_storage_counter") ||
          src_pgv->get ("storage_counter");
      } else {
        ver_fields->user_storage_counter = 0; // Deleted by new_version_fixup.
      }
      ver_fields->storage_counter = REP.get_counter ("storage_counter")->get();
    }
    infant->set_fields (ver_fields);
    infant->parent_version = src_pgv;
    infant->new_version_fixup (
      (< DONT_REUSE_STORAGE, OVERWRITEABLE_STORAGE >)[reuse_storage]);
  }

  else {
    // Fix pg_volatile for the infant. Cannot (and need not) put it in
    // the global print_db->pg_volatiles until it gets a page_group_id
    // (handled in PGVersion.low_set_raw_unver).
    infant->pg_volatile = ([]);

    // Ensure set_fields always is called so various fields get their
    // default values.
    infant->set_unver_fields (([ "edition_id": edition->id() ]));
    infant->set_fields (([]));
  }

  return infant;
}

REP.PGVersion create_page_group (REP.Edition edition,
                                 mapping(string:mixed) unver_fields,
                                 void|mapping(string:mixed) ver_fields,
                                 void|REP.OnError on_value_error)
//! Creates a page group in the given edition with an initial page
//! version with its own storage directory.
//!
//! The page version is not automatically made current. Use
//! @[make_pgv_current] for that.
//!
//! @[unver_fields] and @[ver_fields] contain the initial for the
//! record fields in the unversioned storage (i.e. the page_groups
//! table) and the versioned storage (page_versions), respectively.
//!
//! If @expr{@[unver_fields]->page_group_uuid@} is given then there
//! must not be a page group with that uuid already. An exception is
//! however if the page group belongs to the same edition and has no
//! current page version, in which case it is simply changed.
//!
//! @seealso
//! @[add_pgv], @[set_in_current_pgv]
{
  REP.PGVersion infant = low_make_infant_pgv (0, 0, edition);

  ASSERT_IF_DEBUG (!unver_fields->page_group_id);
  ASSERT_IF_DEBUG (!unver_fields->edition_id);
  ASSERT_IF_DEBUG (!unver_fields->cur_pgversion_id);

  if (unver_fields->page_group_uuid) {
    // If there already is an empty page group with this uuid for this
    // edition then we just use it, since it could occur due to races.
    array(mapping(string:mixed)) pgs =
      page_groups_table()->cached_select (({"    page_group_uuid = %s "
                                            "AND edition_id = %d "
                                            "AND cur_pgversion_id IS NULL",
                                            unver_fields->page_group_uuid,
                                            edition->id()}));
    ASSERT_IF_DEBUG (sizeof (pgs) <= 1);
    if (sizeof (pgs))
      unver_fields->page_group_id = pgs[0];
  }
  if (!infant->set_unver_fields (unver_fields, on_value_error))
    return 0;

  if (ver_fields && !infant->set_fields (ver_fields, on_value_error))
    return 0;

  return page_versions_table()->low_object_create (infant);
}

void assign_pgv_to_slots (REP.PGVersion pgv,
                          array(REP.PageSlot) slots,
                          void|REP.Edition.Section section_hint)
//! Assign the pages in @[pgv] to the given slots. If there are more
//! pages than slots then the remaining pages will be assigned to
//! empty slots after the last given slot, or new slots at the end of
//! the edition (transitional measure until the client handles the
//! scratch area). If there are more slots than pages then unconnected
//! pages are added to @[pgv] which are assigned to the extra slots.
//!
//! @[section_hint] will be used if no slots are provided and we have to
//! find a suitable section to add slots to.
{
  array(REP.Page) pages = pgv->pages();
  REP.PageSlot last_slot;
  int i;

  if (sizeof(slots))
    section_hint = 0;

  for (i = 0; i < sizeof (pages); i++) {
    REP.Page page = pages[i];
    REP.PageSlot slot;

    if (i < sizeof (slots)) {
      slot = slots[i];
      page->set_fields ((["page_slot": slot]));
    }
    else {
      page->set_fields ((["page_slot": last_slot,
                          "_next_empty_page_slot": 1,
                          "_next_empty_page_slot_section": section_hint ]));
      slot = page->page_slot();
    }

    last_slot = slot;
  }

  if (i < sizeof (slots)) {
    array(mapping(string:mixed)) new_pages = ({});
    for (; i < sizeof (slots); i++)
      new_pages += ({(["page_slot": slots[i]])});
    pgv->add_pages (new_pages);
  }
}

REP.PGVersion copy_page_group_fields (
  REP.PGVersion src_pgv,
  REP.Edition edition,
  void|mapping(string:mixed) unver_fields,
  void|mapping(string:mixed) ver_fields,
  void|REP.OnError on_value_error)
{
  unver_fields = src_pgv->get_rec_unver() -
    ({"page_group_id", "edition_id", "cur_pgversion_id", "page_group_uuid",
      "is_deleted", "delete_at", "date_created" }) +
    (["is_edit_locked": 0]) +
    (unver_fields || ([]));
  ver_fields = src_pgv->get_rec() -
    ({"page_version_id", "storage_counter", "user_storage_counter",
      "page_group_id", "page_status_id", "comment", "pages", "text_flow_links",

      //  Fields that are version-dependent that new_version_fixup()
      //  will clear in the infant, and that we don't wish to overwrite.
      //  They will be regenerated in IDS document processing.
      "markup_error", "preflight_result", "file_md", "indesign_version",
      "placements_bucket_id"
    }) +
    (ver_fields || ([]));

  REP.PGVersion new_pgv = REP.DB.create_page_group (edition, unver_fields,
                                                    ver_fields, on_value_error);

  // Copy page group categories
  Sql.Sql sql = REP.get_db();
  sql->query("INSERT INTO page_categories "
             "            (page_group_id, is_main, is_inherited, "
             "             page_category) "
             "    (SELECT %d, is_main, is_inherited, page_category "
             "       FROM page_categories "
             "      WHERE page_group_id = %d) ",
             new_pgv->get ("page_group_id"),
             src_pgv->get ("page_group_id"));

  return new_pgv;
}

REP.PGVersion copy_page_group (REP.PGVersion src_pgv,
                               REP.Edition edition,
                               void|mapping(string:mixed) unver_fields,
                               void|mapping(string:mixed) ver_fields,
                               void|REP.OnError on_value_error,
                               void|int dont_copy_layout_file,
                               void|int dont_copy_stories)
//! Creates a new page group in @[edition] that is a copy of
//! @[src_pgv]. @[unver_fields] and @[ver_fields] can be used to
//! override specific fields in the copy. It is not made current.
{
  REP.PGVersion new_pgv = copy_page_group_fields (src_pgv, edition,
                                                  unver_fields, ver_fields,
                                                  on_value_error);
  if (!new_pgv) return 0;

  array(mapping(string:mixed)) new_pages =
    map (src_pgv->pages(),
         lambda (REP.Page page) {
           return page->get_rec() - ({"page_id", "page_slot_id"});
         });
  // Could avoid another db query here if we could get the infant from
  // create_page_group.
  new_pgv->add_pages (new_pages);

  if (!dont_copy_layout_file) {
    // Only copy the UNPROC file and let the InDesign server do its work
    // (update page numbers/page variables, extract templates and other
    // things that differ between page groups).

    if (string src_path =
        src_pgv->get_existing_layout_filepath (REP.RETURN_ZERO))
      new_pgv->store_file_by_copy (REP.get_print_module()->get_file_db_path() +
                                   src_path,
                                   REP.Storage.UNPROC_LAYOUT_FILE,
                                   src_pgv->layout_mime_type(),
                                   UNDEFINED,
                                   UNDEFINED,
                                   src_pgv);
  }

  if (!dont_copy_stories) {
    REP.DB.copy_stories_between_page_groups (src_pgv, new_pgv);
  }

  return new_pgv;
}

enum ReuseStoragePolicy {
  DONT_REUSE_KEEP_METADATA = -2,
  OVERWRITEABLE_STORAGE = -1,
  DONT_REUSE_STORAGE = 0,
  REUSE_STORAGE = 1,
}

REP.PGVersion add_pgv (REP.PGVersion old_pgv, int reuse_storage,
                       void|mapping(string:mixed) changed_fields,
                       void|REP.OnError on_value_error)
//! Makes a new page version based on an old one.
//!
//! The new page version is not automatically made current. Use
//! @[make_pgv_current] for that. @[set_in_current_pgv] might be more
//! convenient if no new storage is needed.
//!
//! @param old_pgv
//! The basis for the new page version.
//!
//! @param reuse_storage
//! If equal to 1 then the new page version uses the same storage as
//! the old one, if equal to 0 it gets its own storage. If equal to
//! -1, it gets its own storage but is flagged as overwriteable in
//! case of a content conflict (for example, a PGVersion created as a
//! result of repagination can be overwritten since pagination will
//! take place on the newly created PGVersion as well.)
//!
//! @param changed_fields
//! Changes in versioned fields wrt @[old_pgv].
//!
//! @param on_value_error
//! How to report errors from the @[change_fields] setter.
//!
//! @note
//! All fields are set using the high-level setters @expr{set_fields@}.
//!
//! @seealso
//! @[create_page_group]
{
  ASSERT_IF_DEBUG (old_pgv);

  REP.PGVersion infant = low_make_infant_pgv (old_pgv, reuse_storage);

  if (changed_fields && !infant->set_fields (changed_fields, on_value_error))
    return 0;

  REP.PGVersion new_pgv = page_versions_table()->low_object_create (infant);

  if (reuse_storage) copy_data_field_placements (old_pgv, new_pgv);

  return new_pgv;
}

REP.PGVersion add_pgv_with_files (REP.PGVersion old_pgv,
                                  multiset(REP.Storage.FileType) keep_types,
                                  void|mapping(string:mixed) changed_fields,
                                  void|REP.OnError on_value_error)
//! Like add_pgv, but doesn't reference the same storage. Instead,
//! only the @[REP.Storage.FileType]:s specified by keep_types will be
//! copied from @[old_pgv].
{
  ReuseStoragePolicy reuse_storage = DONT_REUSE_STORAGE;

  // Imply that the new version is overwriteable if we're copying the
  // layout files. Logically, that means the versions have identical
  // contents.
  if (keep_types[REP.Storage.UNPROC_LAYOUT_FILE] &&
      keep_types[REP.Storage.LAYOUT_FILE])
    reuse_storage = DONT_REUSE_KEEP_METADATA;

  REP.PGVersion new_pgv = add_pgv (old_pgv, reuse_storage, changed_fields,
                                   on_value_error);

  if (keep_types[REP.Storage.UNPROC_LAYOUT_FILE] &&
      keep_types[REP.Storage.LAYOUT_FILE])
    copy_data_field_placements (old_pgv, new_pgv);

  array(REP.Storage.FileType) ordered_types = (array)keep_types;
  sort (map (ordered_types,
             lambda (REP.Storage.FileType type)
             { // Kludge: SPLIT_LAYOUT_FILE needs to be copied after
               // LAYOUT_FILE due to how get_page_filepath works
               // internally.
               if (type == REP.Storage.SPLIT_LAYOUT_FILE)
                 return 1;

               return REP.Storage.reverse_dependency_order[type];
             }),
        ordered_types);

  string base_db_path = REP.Storage.file_db_path();

  string layout_mime = old_pgv->layout_mime_type();
  string unproc_mime = old_pgv->unproc_mime_type();

  foreach (ordered_types, REP.Storage.FileType type) {
    foreach (old_pgv->get_page_files (type, 1), string src_db_path) {
      int|REP.Page|string subspec;
      if ((< REP.Storage.UNPROC_LAYOUT_FILE, REP.Storage.LAYOUT_FILE>)[type]) {
        //  FIXME: Is this identical to new_pgv->layout_mime_type()? If so,
        //         remove this if branch.
        subspec =
          (type == REP.Storage.UNPROC_LAYOUT_FILE) ? unproc_mime : layout_mime;
      } else {
        //  Ask new pgv to fetch subspec for all other types
        subspec = new_pgv->get_page_file_subspec(src_db_path);
      }
      new_pgv->store_file_by_copy (base_db_path + src_db_path,
                                   type,
                                   subspec,
                                   UNDEFINED,
                                   UNDEFINED,
                                   (type == REP.Storage.UNPROC_LAYOUT_FILE ?
                                    old_pgv :
                                    UNDEFINED));
    }
  }

  return new_pgv;
}

REP.PGVersion add_pgv_except_files (REP.PGVersion old_pgv,
                                    multiset(REP.Storage.FileType) exclude_types,
                                    void|mapping(string:mixed) changed_fields,
                                    void|REP.OnError on_value_error)
//! Like add_pgv_with_files, but copies all file types _except_ those
//! in @[exclude_types].
{
  return add_pgv_with_files (old_pgv,
                             old_pgv->list_file_types() - exclude_types,
                             changed_fields,
                             on_value_error);
}

int(0..1) make_pgv_current (REP.PGVersion new_pgv,
                            void|REP.PGVersion old_pgv,
                            void|REP.OnError on_stale_pgv)
//! Makes @[new_pgv] become the current page group version for the
//! page by setting page_groups.cur_pgversion_id.
//!
//! The current page version must be @[old_pgv] (i.e. zero if
//! @[new_pgv] is the first version), otherwise it's an error which is
//! signalled according to @[on_stale_pgv].
//!
//! If @[old_pgv] is undefined then @[new_pgv] is simply made current.
//! That means an unavoidable race condition. If @[new_pgv] already is
//! current or older, then nothing is done and the function returns
//! successfully.
{
  TableLock lock = TableLock (table_locks_for_pgv_and_page_changes);
  int ok = unlocked_make_pgv_current (new_pgv, old_pgv, on_stale_pgv);
  destruct (lock);
  return ok;
}

int(0..1) unlocked_make_pgv_current (REP.PGVersion new_pgv,
                                     void|REP.PGVersion old_pgv,
                                     void|REP.OnError on_stale_pgv)
{
  ASSERT_IF_DEBUG (!old_pgv || (old_pgv->get ("page_group_id") ==
                                new_pgv->get ("page_group_id")));
  ASSERT_IF_DEBUG (old_pgv != new_pgv);
  ASSERT_IF_DEBUG (!new_pgv->is_live);
  ASSERT_IF_DEBUG (!old_pgv || old_pgv->id() /* %O */ < new_pgv->id() /* %O */,
                   old_pgv, new_pgv);

  REP.PGVersion cur_pgv = new_pgv->current_pgv();
  if (!zero_type (old_pgv) && old_pgv != cur_pgv)
    return REP.raise_err (on_stale_pgv, "Cannot make stale "
                          "page group version %O current "
                          "(latest is %O but expected %O).\n",
                          new_pgv, cur_pgv, old_pgv);

  if (cur_pgv && new_pgv->id() <= cur_pgv->id())
    return 1;

  REP.DB.page_groups_table()->cached_update (
    (["page_group_id": new_pgv->get ("page_group_id"),
      "cur_pgversion_id": new_pgv->id()]));
  DO_IF_DEBUG (new_pgv->is_live = 1);

  new_pgv->internal_update_pages (1);
  new_pgv->invalidate_appearances();

  // Check if we need to mark some old versions as deleted.
  RoxenModule print_db = REP.get_print_module();
  int limit = print_db->query("item_version_limit");
  if (limit > 0) {
    mixed uid = new_pgv->get("user_id");
    int is_null_uid =
      (uid == Roxen.null) ||
      zero_type(new_pgv->get("user_id"));

    array(int) versions = page_versions_table()->
      select1("page_version_id",
              ("is_deleted = 0 AND " +
               (!is_null_uid ?
                "user_id = " + uid :
                "user_id IS NULL") + " AND "
               "page_group_id = " + new_pgv->get("page_group_id") + " AND "
               "page_version_id <= " + new_pgv->id()),
              UNDEFINED, "ORDER BY page_version_id DESC")[limit..];
    foreach(versions, int page_version_id) {
      // Could consider optimizing this for objects not in the cache
      // by only updating the db, but then we need to address the race
      // that occurs if an object is created from another thread.
      if (REP.PGVersion pgv = pgv_by_id (page_version_id, REP.RETURN_ZERO))
        pgv->delete (1);
    }
  }

  page_versions_table()->
    schedule_record_notification ("new_version", new_pgv, cur_pgv);

  new_pgv->parent_version = 0;

  if (!cur_pgv ||
      (cur_pgv->get("storage_counter") != new_pgv->get("storage_counter"))) {
    // The layout file has changed.
    new_pgv->edition()->bump_page_changed();
  }

  return 1;
}

REP.PGVersion set_in_current_pgv (
  REP.PGVersion pgv,
  mapping(string:mixed) fields,
  void|mapping(REP.Page:mapping(string:mixed)) page_fields,
  void|int remove_pages,
  void|array(mapping(string:mixed)) add_pages,
  void|mapping(string:mixed) unver_fields,
  void|REP.OnError on_value_error)
//! Sets fields in the current page version for the page group. I.e.
//! creates a new page version with reused storage, do the changes,
//! and makes it current. The operation is atomic.
//!
//! @param pgv
//! One of the @[REP.PGVersion]s for the page group.
//!
//! @param fields
//! Versioned fields to set. May be zero to skip this.
//!
//! @param page_fields
//! Can be used to change fields in the pages or delete them. For each
//! @[REP.Page] in the index, the fields are set according to the
//! value. The page objects need not belong to the current version
//! (but the same page must exist there).
//!
//! @param remove_pages
//! If nonzero, then this many pages are removed from the page group.
//! Pages are always removed from the end according to document order.
//!
//! @param add_pages
//! Use this to add new pages with the given initial values.
//!
//! @param unver_fields
//! Unversioned fields to set.
//!
//! @param on_value_error
//! Passed on to the @expr{on_value_error@} argument in each
//! @expr{set_fields@} call.
//!
//! @returns
//! Returns the new current page group version. Returns zero iff any
//! @expr{set_fields@} call returned zero (no change is done in that
//! case).
//!
//! @note
//! All fields are set using the high-level setters @expr{set_fields@}.
{
  // Duplicated from PGVersion.set_fields. Do this before the
  // TableLock to avoid having to lock page_types and page_status.
  if (string page_type = fields && m_delete (fields, "page_type"))
    fields->page_type_id = REP.DB.get_page_type_id (page_type);
  if (string page_status = fields && m_delete (fields, "page_status"))
    fields->page_status_id = REP.DB.get_page_status_id (page_status);

  TableLock lock = TableLock (table_locks_for_pgv_and_page_changes);

  return unlocked_set_in_current_pgv (pgv, fields, page_fields, remove_pages,
                                      add_pages, unver_fields, on_value_error);
}

REP.PGVersion unlocked_set_in_current_pgv (
  REP.PGVersion pgv,
  mapping(string:mixed) fields,
  void|mapping(REP.Page:mapping(string:mixed)) page_fields,
  void|int remove_pages,
  void|array(mapping(string:mixed)) add_pages,
  void|mapping(string:mixed) unver_fields,
  void|REP.OnError on_value_error)
{
  ASSERT_IF_DEBUG (!fields || zero_type (fields->page_type));
  ASSERT_IF_DEBUG (!fields || zero_type (fields->page_status));

  REP.PGVersion cur_pgv = pgv->current_pgv();
  REP.PGVersion infant = low_make_infant_pgv (cur_pgv, 1);

  if (fields && !infant->set_fields (fields, on_value_error))
    return 0;

  foreach (page_fields || ([]); REP.Page page; mapping(string:mixed) fields) {
    ASSERT_IF_DEBUG (fields /*for page %O*/, page);
    if (REP.Page new_page = infant->page_by_id (page->id())) {
      if (!new_page->set_fields (fields, on_value_error))
        return 0;
    }
    else
      return REP.raise_err (on_value_error,
                            "Page %O does not exist in current version %O.\n",
                            page, cur_pgv);
  }

  if (remove_pages)
    infant->remove_last_pages (remove_pages);

  if (add_pages) {
    if (!infant->add_pages (add_pages, on_value_error))
      return 0;
  }

  REP.PGVersion new_pgv = page_versions_table()->low_object_create (infant);

  copy_data_field_placements (cur_pgv, new_pgv);

  // Room for optimization: Combine the db updates made by
  // set_unver_fields and unlocked_make_pgv_current.

  if (unver_fields &&
      !new_pgv->set_unver_fields (unver_fields, on_value_error)) {
    // Let the gc clean up the new pgv.
    new_pgv->delete(1);
    return 0;
  }

  unlocked_make_pgv_current (new_pgv, cur_pgv);

  // Disabled for now. See [Bug 6291].
  //  cur_pgv->delete(1);
  return new_pgv;
}

//! Returns an array of "mapping of text chain properties" for all
//! text chains for an edition. Intended for e.g. JSON serialization,
//! to be sent to the InDesign plugin or similar.
array(mapping(string:mixed))
ext_text_chains_for_edition (REP.Edition edition)
{
  array(mapping(string:mixed)) ext_chains = ({});
  mapping(string:array(REP.TextFlowLink)) text_chains =
    edition->get_text_chains();

  foreach (text_chains; string uuid; array(REP.TextFlowLink) tfls) {
    REP.TextFlowLink first_tfl = sizeof(tfls) && tfls[0];
    array(REP.Story) stories = first_tfl && first_tfl->stories_in_link();

    mapping(int:int(1..1)) outdated_pgv_ids = ([ ]);
    foreach (tfls, REP.TextFlowLink tfl) {
      [int update_type, REP.PGVersion src_pgv,
       REP.Storage.FileType src_snippet] = tfl->reflow_source();
      if (update_type == 1) {
        outdated_pgv_ids[tfl->current_pgv()->id()] = 1;
      }
    }

    mapping(string:mixed) entry =
      ([ "is_rep_text_chain": 1,
         "uuid": uuid,
         "stories": map(stories || ({ }),
                        lambda(REP.Story story) {
                          //  *** FIXME [JUMPS]: Bad idea to inline story
                          //      data here?
                          //      If e.g. title or planned_pageno changes,
                          //      will we send a push? Client may wish to
                          //      xref in separate stories table instead.
                          //
                          //      We don't push CMT.planner_text_chains
                          //      on story changes today so title changes
                          //      are not sent (and possibly neither are
                          //      appearance changes).
                          //
                          //      NOTE: "object_type" and "unique_object_id"
                          //      are excluded here to ensure a proxy object
                          //      is created correctly client-side.
                          mapping story_fields = ([ "story_id": 1,
                                                    "story_uuid": 1,
                                                    "story_tmpl_def_id": 1,
                                                    "story_tmpl_name": 1,
                                                    "title": 1,
                                                    "status_level": 1,
                                                    "story_categories": 1,
                                                    "jump_title": 1,
                                                    "date_modified": 1,
                                                    "item_group": 1,
                                                    "assignees": 1,
                                                    "overflows": 1 ]);

                          mapping story_res =
                            story->rec_from_get_ext(story_fields) +
                            ([ "content_hash": story->content_hash() ]);

                          //  Collect info about each jump in the chain.
                          //  This is needed because the user may focus on
                          //  any of the jumps in Page Composer (when
                          //  editing the corresponding page).
                          //
                          //  We include PGV id and slot name of the slot
                          //  touched by the story as well as other data
                          //  related to the snippet.
                          array(array(mapping)) jump_info = ({ });
                          foreach (tfls, REP.TextFlowLink tfl) {
                            if (REP.PageSlot last_ps =
                                tfl->get_last_page_slot(story)) {
                              REP.PGVersion pgv = tfl->current_pgv();
                              int pg_id = pgv->get("page_group_id");
                              int has_os =
                                tfl->get("overflow_snippet") ? 1 : 0;
                              mapping counters =
                                tfl->get("overflow_counters") || 0;
                              jump_info += ({
                                  ([ "pgv_id": pgv->id(),
                                     "slot_name": last_ps->page_name(),
                                     "has_overflow_snippet": has_os,
                                     "overflow_counters": counters ])
                                });
                            }
                          }
                          if (sizeof(jump_info))
                            story_res["jump_info"] = jump_info;

                          return story_res;
                        }),
         "outdated_pgv_ids": indices(outdated_pgv_ids)
      ]);

    //  All entries at this point should represent article/body#1 fields
    //  only. We don't need to list present fields separately.
    ext_chains += ({ entry });
  }

  return ext_chains;
}


// Pages

//! Constant mapping to feed to @[TableLock] to lock all tables
//! required to add page versions and change page assignments to page
//! slots.
constant table_locks_for_pgv_and_page_changes = ([
  // Placement invalidations
  "ads": READ_LOCK,
  // Write lock on page_versions for direct changes, and also since
  // internal_update_pages may cause placeholder pgv's to be created.
  "page_versions": WRITE_LOCK,
  // To change cur_pgversion_id.
  "page_groups": WRITE_LOCK,
  // For changes in the fields of current pages (when new page
  // versions are made current).
  "pages": WRITE_LOCK,
  // Edition lock necessary for Edition.bump_page_changed.
  "editions": WRITE_LOCK,
  // Edition may need to look up its parent Publication.
  "publications": READ_LOCK,
  // internal_update_pages may access slots, and create placeholder slots.
  "page_slots": WRITE_LOCK,
  // internal_update_pages may remap story appearances.
  "story_app_parts": WRITE_LOCK,
  "story_apps": WRITE_LOCK,
  // is_placeholder called from delete_placeholder_pgv access these tables.
  "page_categories": READ_LOCK,
  // The permission check in PGVersion.set_fields needs the page status value.
  "page_status": READ_LOCK,
  "stories": READ_LOCK,
  // copy_data_field_placements may call SIVersion.update_df_placements_cache.
  "story_items": WRITE_LOCK,
  // copy_data_field_placements may call REP.DB.siv_by_id.
  "story_item_versions": READ_LOCK,
  // PGVersion.set_fields requests a counter number for the
  // storage counter if a placeholder pgv is created.
  "counters": WRITE_LOCK,
  // unlocked_set_in_current_pgv calls copy_data_field_placements.
  "data_field_placements": WRITE_LOCK,
  // create_infant calls set_default_user_properties.
  "user_property_defs": READ_LOCK,
  // PGVersion may update a bucket owner if storage counter is unchanged
  "buckets": WRITE_LOCK,
  // Placement invalidations
  "bucket_items": WRITE_LOCK,
]);

//! Constant mapping to feed to @[REP.DB.TableLock] to lock all tables
//! required for page slot renumbering.
constant table_locks_for_page_slot_renumber =
  table_locks_for_pgv_and_page_changes;

REP.Page current_page_by_id (int(1..) page_id,
                             void|REP.OnError on_rec_not_found)
//! Returns the page with the given id in a current page group
//! version.
{
  mapping(string:mixed) page_rec =
    pages_table()->cached_get (page_id, on_rec_not_found);
  if (!page_rec) return 0;
  ASSERT_IF_DEBUG (!zero_type (page_rec->page_group_id));

  REP.PGVersion pgv =
    current_pgv_by_pg_id (page_rec->page_group_id, REP.RETURN_ZERO);

  if (!pgv)
    return REP.raise_err (on_rec_not_found, "No page with id %d found "
                          "in a current page version.\n", page_id);

  // If the following doesn't find the page then the pages table is
  // out of sync with the page records. Should only happen due to
  // race, and then it's ok to give up.
  return pgv->page_by_id (page_id, on_rec_not_found);
}

REP.Page current_page_by_ext_source_id (string ext_source_id,
                                        void|REP.Edition edition,
                                        void|REP.OnError on_rec_not_found)
//! Returns the page with the given external source id (often an UUID)
//! in a current page group version. If @[edition] is given, the
//! search is restricted to that edition.
{
  REP.CachedTable pages_tbl = pages_table();
  array(int) res =
    pages_tbl->select1 ("page_id",
                        sprintf ("ext_source_id = '%s'",
                                 pages_tbl->quote (ext_source_id)) +
                        (edition ?
                         sprintf (" AND page_slots.edition_id = %d",
                                  edition->id()) :
                         ""),
                        "INNER JOIN page_slots ON "
                        "(pages.page_slot_id = page_slots.page_slot_id)",
                        "ORDER BY page_slots.is_deleted ASC");
  if (!sizeof (res))
    return REP.raise_err (on_rec_not_found,
                          "No page with ext_source_id %s found.\n",
                          ext_source_id);

  return REP.DB.current_page_by_id (res[0], on_rec_not_found);
}

array(REP.Page) sort_pages_by_page_slot_order (array(REP.Page) pages)
//! Returns an array that contains the given pages sorted by the order
//! of their page slots. Assumes that all given pages are bound to
//! page slots.
//!
//! @seealso
//! @[sort_pages_by_doc_order], @[sort_pages_by_page_order],
//! @[sort_page_slots_by_order], @[sort_pgvs_by_page_slot_order]
{
  ASSERT_IF_DEBUG (!sizeof (pages) ||
                   !sizeof (pages->pgv()->edition() -
                            ({pages[0]->pgv()->edition()})));

  array(int) order = pages->page_slot()->get ("page_order");
  pages += ({});
  sort (order, pages);
  return pages;
}

array(REP.Page) sort_pages_by_doc_order (array(REP.Page) pages)
//! Returns an array containing the given pages ordered first by
//! page_group_id and then by their doc_order.
//!
//! @seealso
//! @[sort_pages_by_page_slot_order], @[sort_pages_by_page_order]
{
  ASSERT_IF_DEBUG (!sizeof (pages) ||
                   !sizeof (pages->pgv()->edition() -
                            ({pages[0]->pgv()->edition()})));

  return Array.sort_array (
    pages,
    lambda (REP.Page a, REP.Page b) {
      int pg_a = a->pgv()->get ("page_group_id");
      int pg_b = b->pgv()->get ("page_group_id");
      return pg_a > pg_b || (pg_a == pg_b &&
                             a->get ("doc_order") > b->get ("doc_order"));
    });
}

array(REP.Page) sort_pages_by_page_order (array(REP.Page) pages)
//! Returns an array containing the given pages ordered by page order:
//! All pages bound to page slots come first, sorted by
//! @[sort_pages_by_page_slot_order]. Then comes the remaining pages
//! sorted by @[sort_pages_by_doc_order].
{
  ASSERT_IF_DEBUG (!sizeof (pages) ||
                   !sizeof (pages->pgv()->edition() -
                            ({pages[0]->pgv()->edition()})));

  array(REP.Page) bound = ({}), unbound = ({});
  array(int) bound_order = ({});
  foreach (pages, REP.Page page) {
    if (REP.PageSlot slot = page->page_slot()) {
      bound += ({page});
      bound_order += ({slot->get ("page_order")});
    }
    else
      unbound += ({page});
  }

  sort (bound_order, bound);

  if (sizeof (unbound))
    unbound = sort_pages_by_doc_order (unbound);

  return bound + unbound;
}


// Page slots

Thread.MutexKey page_slot_order_lock (void|int lock_flag,
                                      void|int no_sql_query_while_locked)
//! Locks a mutex that covers the page order and page renumbering
//! related stuff in @[REP.Edition] and @[REP.PageSlot].
//!
//! Lock order: Must have a @[TableLock] on
//! @[table_locks_for_page_slot_renumber] before this one, unless it
//! is guaranteed that no SQL queries will take place during the scope
//! of this lock (which is signalled by the
//! @[no_sql_query_while_locked] argument). The @[TableLock] must
//! never be locked after this one, regardless of SQL queries (usual
//! lock order semantics).
//!
//! Additional note on lock order: the reason that
//! @[table_locks_for_page_slot_renumber] must be locked if an SQL
//! query takes place during the scope of this lock is that a deadlock
//! might occur otherwise, in the following scenario:
//!
//! Thread 1                            Thread 2
//! Locks TableLock
//!                                     Locks page_slot_order_lock
//! Waits for page_slot_order_lock
//!                                     SQL query on a TableLocked table
//!
{
  ASSERT_IF_DEBUG (no_sql_query_while_locked ||
                   got_table_lock (table_locks_for_page_slot_renumber));
  return REP.get_print_module()->page_slot_order_mutex->lock (lock_flag);
}

Thread.MutexKey page_slot_order_trylock()
//! Try-lock variant of @[page_slot_order_lock].
{
  ASSERT_IF_DEBUG (got_table_lock (table_locks_for_page_slot_renumber));
  return REP.get_print_module()->page_slot_order_mutex->trylock();
}

int page_slot_order_is_locked()
//! Returns true iff the current thread got @[page_slot_order_lock].
//! Mostly for debug assertions.
{
  return
    REP.get_print_module()->page_slot_order_mutex->current_locking_thread() ==
    this_thread();
}

REP.PageSlot page_slot_by_id (int(1..) page_slot_id,
                              void|REP.OnError on_rec_not_found)
{
  return page_slots_table()->object_get (page_slot_id, on_rec_not_found);
}

REP.Page create_placeholder_pgv (REP.PageSlot slot)
//! Creates a placeholder page group for the given slot, if it
//! doesn't have a page assigned already.
//!
//! Intended for internal use only; other code can just call
//! @[REP.PageSlot.page], and it will be created if necessary.
{
  ASSERT_IF_DEBUG (slot->id());

  // This lock is heavy artillery, but we'll take it anyway in
  // make_pgv_current, so now we just do it a bit earlier to fix the
  // lock order.
  TableLock lock = TableLock (table_locks_for_pgv_and_page_changes);

  // Lock this mutex to avoid races creating and deleting placeholder
  // pgv's. Could consider a separate mutex for it, but that'd
  // introduce tricky lock order issues. Unfortunately that means we
  // have to allow recursive locking.
  Thread.MutexKey ps_lock = page_slot_order_lock (2);

  if (REP.Page page = slot->low_get_page())
    return page;

  REP.PGVersion infant = low_make_infant_pgv (0, 0, slot->edition());
  infant->add_pages (({(["page_slot_id": slot->id()])}));
  REP.PGVersion new_pgv = page_versions_table()->low_object_create (infant);
  REP.Page page = new_pgv->pages()[0];

  make_pgv_current (new_pgv, 0);

#ifdef DEBUG_SLOT_ASSIGNMENT
  werror ("create_placeholder_pgv for slot %O: %O, page %O\n",
          slot, new_pgv, page);
#endif
  return page;
}

int delete_placeholder_pgv (REP.PGVersion pgv)
//! Deletes the given page group iff it is a placeholder according to
//! @[REP.PGVersion.is_placeholder]. Only intended to be called from
//! @[REP.Page.low_set_raw].
//!
//! @returns
//! Returns true if the page group got deleted.
{
  Thread.MutexKey ps_lock = page_slot_order_lock (2);

  if (!pgv->is_placeholder()) return 0;

#ifdef DEBUG_SLOT_ASSIGNMENT
  werror ("delete_placeholder_pgv: %O, page%{ %O%}\n", pgv, pgv->pages());
#endif
  pgv->set_unver_fields ((["is_deleted": 1]));
  pgv->delete (1);

  return 1;
}


enum CreateShadowMode {
  NO_SHADOW = 0,
  MATCH_SECTION_POS,
  LAST_IN_SECTION
};

enum ShadowCopyPolicy {
  //  No zero value in this enum please
  DEFAULT_SHADOW_POLICY = 1,
  PRESERVE_SHADOW_ADS,
  OVERWRITE_ALL,
  CLEAR_SHADOW_ADS
};

protected void update_shadow_pgvs_cb(REP.REPSession ses)
{
  Concurrent.Promise p = ses->userdata->update_shadow_promise;
  mapping(REP.PGVersion:mapping(string|REP.Edition:ShadowCopyPolicy))
    pgv_queue = ses->userdata->update_shadow_pgvs;
  if (!pgv_queue || !sizeof(pgv_queue)) {
    //  Empty array of pages to process
    p->success( ({ }) );
    return;
  }

  //  Each shadow copy operation may update multiple PGV shadows
  array(Concurrent.Future/*array(REP.PGVersion)*/) futures = ({ });
  foreach (pgv_queue; REP.PGVersion pgv;
           mapping(string|REP.Edition:ShadowCopyPolicy) dst_editions) {
    Concurrent.Future f =
      pgv->update_shadowed_slots(mappingp(dst_editions) && dst_editions);
    futures += ({ f });
  }

  //  Clear session so we don't capture it in the lambda below
  ses = 0;

  //  Collect all new PGVs when IDS processing finishes
  Concurrent.results(futures)->
    flat_map(lambda(array(array(REP.PGVersion)) res) {
        array(REP.PGVersion) new_pgvs = Array.flatten(res);
        p->success(new_pgvs);

        //  Return value not used but needed to avoid internal error
        return Concurrent.resolve(0);
      });
}


Concurrent.Future/*array(REP.PGVersion)*/
queue_update_shadow_pgvs(array(REP.PGVersion) pgvs,
                         void|multiset(REP.Edition)|
                         mapping(string|REP.Edition:ShadowCopyPolicy)
                           dst_editions)
{
  //  Don't copy deleted PGVs or where all slots are deleted. We also
  //  verify Z/E collection since that is a requirement for shadowing.
  pgvs = filter(pgvs, lambda(REP.PGVersion pgv) {
      return
        pgv->edition()->is_ze_edition() &&
        pgv->is_placed();
    });
  if (!sizeof(pgvs))
    return Concurrent.resolve( ({ }) );

  //  In order to avoid redundant shadow updates of the same PGV across
  //  different slots we accumulate all requests in a session callback.
  //
  //  We'll merge a list of editions to target if that is given with
  //  potentially unique policies per copy. If no edition list is given
  //  we'll flag "*" in our queue as placeholder for also copying to any
  //  linked edition.
  REP.REPSession ses = REP.get_session();
  mapping ud = ses->userdata;
  if (!ud->update_shadow_pgvs)
    ud->update_shadow_pgvs = ([ ]);
  mapping(REP.PGVersion:mapping(string|REP.Edition:ShadowCopyPolicy))
    ud_queue = ud->update_shadow_pgvs;

  if (!dst_editions)
    dst_editions = ([ "*": DEFAULT_SHADOW_POLICY ]);
  else if (multisetp(dst_editions)) {
    dst_editions =
      mkmapping(indices(dst_editions),
                ({ DEFAULT_SHADOW_POLICY }) * sizeof(dst_editions));
  }

  ASSERT_IF_DEBUG(mappingp(dst_editions));

  foreach (pgvs, REP.PGVersion pgv) {
    ud_queue[pgv] = (ud_queue[pgv] || ([ ]) ) + dst_editions;
  }

  //  Reuse existing promise or create a new one if none is found
  ud->update_shadow_promise =
    ud->update_shadow_promise ||
    Concurrent.Promise();

  ses->register_uniq_session_done_callback(update_shadow_pgvs_cb);
  return ud->update_shadow_promise->future();
}


REP.PageSlot find_next_shadow_parent_sibling(REP.PageSlot shadow_ps)
{
  //  For a shadowed slot, try to see if it's part of a PGV where the
  //  top parent Z/E has a next sibling. If so, return it.
  //
  //  The reason top parent and not closest parent is used is that an
  //  intermediate zone may have deleted that shadowed slot and thus
  //  hides the link to the slot we expect to find.
  if (REP.PageSlot parent_ps = shadow_ps->shadow_top_parent()) {
    if (REP.Page parent_p = parent_ps->low_get_page()) {
      array(REP.PageSlot) parent_pss =
        parent_p->pgv()->pages()->page_slot() - ({ 0 });
      int parent_ps_idx = search(parent_pss, parent_ps);
      if ((parent_ps_idx >= 0) &&
          (parent_ps_idx < (sizeof(parent_pss) - 1))) {
        return parent_pss[parent_ps_idx + 1];
      }
    }
  }
  return UNDEFINED;
}


REP.PageSlot find_next_shadow_sibling(REP.PageSlot shadow_ps)
{
  //  For a shadowed slot, try to see if it's part of a PGV where the
  //  top parent Z/E has a next sibling with a shadow in our section.
  //
  //  This is used when page groups are extended (e.g. multi-page files
  //  are uploaded or when pages are added directly in InDesign), or other
  //  actions such as join.
  if (REP.PageSlot parent_next_ps =
      find_next_shadow_parent_sibling(shadow_ps)) {
    //  We have found the next sibling slot in the parent PGV. Now see if
    //  that has a shadow slot in our current edition.
    array(REP.PageSlot) cand_pss = parent_next_ps->shadowed_slots(true);
    REP.Edition shadow_ed = shadow_ps->edition();
    REP.Edition.Section shadow_section = shadow_ps->section();
    foreach (cand_pss, REP.PageSlot cand_ps) {
      if ((cand_ps->edition() == shadow_ed) &&
          (cand_ps->section() == shadow_section)) {
        //  Found a match!
        return cand_ps;
      }
    }
  }
  return UNDEFINED;
}


array(REP.PageSlot) unlocked_create_shadow_slots(REP.PageSlot parent_ps,
                                                 CreateShadowMode
                                                   create_shadow_mode)
{
  //  Collect all transitive edition branches and create all slots at once.
  //  Locked editions need to be ignored, and shadows that would have been
  //  linked there instead have to be linked to the closest unlocked parent
  //  in the graph.
  //
  //  We offer two strategies for where new slots are placed:
  //
  //    MATCH_SECTION_POS:
  //
  //      Finds the same section (by ref or marker) in the branched editions
  //      and then looks for identical prev/next slot names to match the
  //      position. We don't use slot index since the branches could have
  //      different section size.
  //
  //      We could theoretically identify the given slot's before and/or
  //      after sibling and trace their shadow graphs to find corresponding
  //      locations in the branched editions, but that is risky given that
  //      some slots may have been unlinked. Instead we use a strategy based
  //      on section and page name of prev/next slot.
  //
  //    LAST_IN_SECTION:
  //
  //      This strategy simply adds the shadow slots last in the matching
  //      section.
  REP.Edition parent_ed = parent_ps->edition();
  if (!parent_ed->is_ze_edition() || parent_ps->get("is_deleted"))
    return 0;

  //  Build lookup table of the Z/E graph
  array(REP.Edition) ze_coll = parent_ed->ze_collection();
  mapping(int:array(REP.Edition)) ze_children = ([ ]);
  foreach (ze_coll, REP.Edition ze_ed) {
    if (int ze_parent_id = ze_ed->get("ze_parent_edition_id"))
      ze_children[ze_parent_id] += ({ ze_ed });
  }

  //  Returns a tuple < section, after_slot > for use in placing a new slot
  //  in the given edition. The parent slot will point to a different (parent)
  //  edition and is used to identify the section and neighboring slots.
  array(REP.Edition.Section|REP.PageSlot)
  find_after_slot(REP.PageSlot parent_ps, REP.Edition shadow_ed) {
    //  Get parent slot's section reference (if any) and map to counterpart
    //  in the current edition.
    REP.Edition.Section shadow_section;
    if (!shadow_ed->has_fallback_section() &&
        !parent_ps->edition()->has_fallback_section()) {
      //  Only if both editions use proper sections is there a point to map
      //  from one to the other.
      REP.Edition.SectionDef parent_section_def = parent_ps->section_def();
      shadow_section = shadow_ed->get_section_by_ref(parent_section_def->ref());
      if (!shadow_section) {
        //  Couldn't find match using ref field so try the section marker
        string parent_section_marker = parent_section_def->marker();
        foreach (shadow_ed->all_sections(),
                 REP.Edition.Section cand_shadow_section) {
          if (cand_shadow_section->def()->marker() == parent_section_marker) {
            shadow_section = cand_shadow_section;
            break;
          }
        }
      }
    }
    if (!shadow_section)
      shadow_section = shadow_ed->main_section();

    array(REP.PageSlot) cand_pss = shadow_section->page_slots();
    if (!sizeof(cand_pss))
      return ({ shadow_section, 0 });

    switch (create_shadow_mode) {
    case LAST_IN_SECTION:
      return ({ shadow_section, cand_pss[-1] });

    case MATCH_SECTION_POS:
      //  Try to match the parent slot's page name and relative position
      //  within its section.
      array(REP.PageSlot) parent_pss = parent_ps->section()->page_slots();
      array(string) parent_pss_names = parent_pss->page_name();
      array(string) cand_pss_names = cand_pss->page_name();
      int parent_ps_idx = search(parent_pss, parent_ps);

      ASSERT_IF_DEBUG(parent_ps_idx >= 0);

      if (parent_ps_idx > 0) {
        //  Match preceding slot
        int shadow_prev_idx =
          search(cand_pss_names, parent_pss_names[parent_ps_idx - 1]);
        if (shadow_prev_idx >= 0) {
          return ({ shadow_section, cand_pss[shadow_prev_idx] });
        }

        //  Match following slot
        if (parent_ps_idx < (sizeof(parent_pss) - 1)) {
          int shadow_next_idx =
            search(cand_pss_names, parent_pss_names[parent_ps_idx + 1]);
          if (shadow_next_idx > 0)
            return ({ shadow_section, cand_pss[shadow_next_idx - 1] });
        }

        //  Cannot match so place last in section
        return ({ shadow_section, cand_pss[-1] });
      } else if (parent_ps_idx == 0) {
        //  First in section so fallback is ok
      }
      break;

    default:
      error("Unknown shadow slot placement strategy: " + create_shadow_mode);
    }

    return ({ shadow_section, 0 });
  };

  REP.PageSlot create_shadow(REP.PageSlot parent_slot, REP.Edition shadow_ed) {
    //  Identify section and best position where the new slot should be
    //  inserted.
    [ REP.Edition.Section shadow_section, REP.PageSlot after_ps ] =
      find_after_slot(parent_slot, shadow_ed);
    mapping(string:mixed) fields = ([
      "after_slot": after_ps,
      "shadow_parent_id": parent_slot->id()
    ]);
    REP.PageSlot shadow_ps =
      unlocked_create_page_slot(shadow_section, fields, REP.RETURN_ZERO, false);
    return shadow_ps;
  };

  void recurse_link_from(REP.Edition parent_ed, REP.PageSlot parent_ps) {
    //  Create links to all editions that branch from the parent edition
    foreach (ze_children[parent_ed->id()] || ({ }), REP.Edition shadow_ed) {
      //  If the target edition is unlocked a new slot should be created and
      //  act as the new parent slot for further recursion. When the edition
      //  is locked we pass our current parent slot instead.
      bool ed_locked = shadow_ed->get("is_locked");
      REP.PageSlot shadow_ps =
        !ed_locked &&
        create_shadow(parent_ps, shadow_ed);
      recurse_link_from(shadow_ed, shadow_ps || parent_ps);
    }
  };

  //  Generate the links
  recurse_link_from(parent_ed, parent_ps);
}


REP.PageSlot create_page_slot (REP.Edition|REP.Edition.Section ed_or_sec,
                               mapping(string:mixed) fields,
                               void|REP.OnError on_value_error,
                               void|CreateShadowMode create_shadow_mode)
//! Creates a page slot (together with a placeholder page group) in
//! the given edition. @[fields] contains the initial values for the
//! record fields.
{
  // FIXME: Flag to delay creating the placeholder, if the caller
  // already has a page to put in the slot.

  // Note code dup in create_page_slots() and copy_page_slot().

  // Heavy lock which we have to take here to ensure lock order. It
  // may otherwise be taken from unlocked_create_page_slot ->
  // internal_update_page_order_cache -> fix_slot_numbering.
  TableLock lock = TableLock (table_locks_for_pgv_and_page_changes);

  // page_slot_order_lock is necessary to ensure that the page_order
  // calculated in the PageSlot.set_fields call is still valid in the
  // later commit.
  Thread.MutexKey ps_lock = page_slot_order_lock();

  return unlocked_create_page_slot (ed_or_sec, fields, on_value_error,
                                    create_shadow_mode);
}

REP.PageSlot unlocked_create_page_slot (REP.Edition|REP.Edition.Section
                                          ed_or_sec,
                                        mapping(string:mixed) fields,
                                        void|REP.OnError on_value_error,
                                        void|CreateShadowMode
                                          create_shadow_mode)
{
  REP.Edition edition =
    ed_or_sec->is_rep_section ? ed_or_sec->edition() : ed_or_sec;
  REP.Edition.Section section =
    ed_or_sec->is_rep_section ? ed_or_sec : edition->main_section();

  ASSERT_IF_DEBUG (edition->id());
  ASSERT_IF_DEBUG (section);

  if (edition->get("is_locked"))
    return 0;

  fields->edition_id = edition->id();
  fields->section_id = section->id();
  REP.PageSlot slot =
    page_slots_table()->object_create (fields, on_value_error);
  if (slot) {
    section->internal_update_page_order_cache (slot);
    create_placeholder_pgv (slot);

    if (create_shadow_mode && edition->is_ze_edition())
      unlocked_create_shadow_slots(slot, create_shadow_mode);
  }
  return slot;
}

array(REP.PageSlot) create_page_slots(REP.Edition|REP.Edition.Section ed_or_sec,
                                      void|int(1..) num_pages,
                                      void|REP.PageSlot after_slot,
                                      void|CreateShadowMode create_shadow_mode)
//! Create new slots in an edition. Optionally at a given position in the
//! edition.
//!
//! @param ed_or_sec
//!    The @[REP.Edition] or @[REP.Edition.Section] in which to create the
//!    new slots. If section isn't given it implies the main section.
//! @param num_pages
//!    Optional number of pages to add. Defaults to 1.
//! @param after_slot
//!    Optional position argument where pages are to be added. If this and
//!    section both are specified the slot must be part of the given section.
{
  array(REP.PageSlot) res = ({ });
  mapping(string:mixed) slot_fields = ([ ]);
  if (!zero_type(after_slot))
    slot_fields += ([ "after_slot" : after_slot ]);

  //  Use same lock as create_page_slot() and call the unlocked version in
  //  the loop. See the function above for comments.
  TableLock lock = TableLock (table_locks_for_pgv_and_page_changes);
  Thread.MutexKey ps_lock = page_slot_order_lock();

  num_pages = num_pages || 1;
  for(; num_pages > 0; num_pages--) {
    after_slot = unlocked_create_page_slot(ed_or_sec, slot_fields + ([ ]),
                                           0, create_shadow_mode);
    slot_fields->after_slot = after_slot;
    res += ({ after_slot });
  }
  return res;
}

REP.PGVersion copy_page_into(REP.PageSlot src_slot, REP.PageSlot dst_slot)
{
  if (src_slot->is_empty())
    return 0;		// Got nothing to copy

  REP.Page src_page = src_slot->page();
  REP.PGVersion src_pgv = src_page->pgv();
  REP.PGVersion dst_pgv = dst_slot->page()->pgv();
  REP.PGVersion new_pgv;

  mapping(string:mixed) changed_fields = ([ ]);
  string comment =
    "Copied from " +
    (src_pgv->edition()->get ("title") || "nameless issue") +
    ", page " + src_slot->page_name();

  if (sizeof (src_pgv->pages()) == sizeof (dst_pgv->pages())) {
    // If the source and dest page groups have the same number
    // of pages, we'll avoid splitting the source to allow
    // copying of e.g. spread documents. In the long run we
    // should probably have some kind of marker/flag that can
    // be used in template documents to instruct the system
    // that a spread is in fact a spread. Also, there is no
    // way to drag and drop a spread onto an empty page slot
    // currently (since the single page will be extracted
    // then.
    new_pgv =
      REP.DB.copy_page_group (src_pgv, dst_slot->edition(), 0, changed_fields);
    array(REP.PageSlot) slots = dst_pgv->pages()->page_slot() - ({ 0 });

    REP.DB.assign_pgv_to_slots (new_pgv, slots);
    new_pgv = REP.DB.add_pgv (new_pgv, 1, ([ "comment": comment ]) );
    REP.DB.make_pgv_current (new_pgv);
  } else {
    // FIXME: Copy spreads from template issues: In a template
    // issue we typically got many template pages, and some
    // should be kept together as spreads. Should have a flag
    // for that and then a way to keep the spreads together.
    string page_file_path = src_page->get_split_layout_filepath (1);
    if (!page_file_path) {
      report_warning ("Failed to retrieve split layout file for %O.\n",
                      src_page);
      return 0;
    }

    if (!dst_pgv->is_placeholder() && sizeof (dst_pgv->pages()) == 1) {
      // Make a new version on the existing page group.
      new_pgv = REP.DB.add_pgv (dst_pgv, 0, ([ "comment": comment ]) );
    } else {
      new_pgv = REP.DB.copy_page_group_fields (src_pgv,
                                               dst_slot->edition(),
                                               0,
                                               changed_fields,
                                               REP.THROW_RXML);

      REP.DB.assign_pgv_to_slots (new_pgv, ({ dst_slot }));
      new_pgv = REP.DB.add_pgv (new_pgv, 0, (["comment": comment]));
    }

    string base_db_path = REP.Storage.file_db_path();

    void finish_page_file_copy(int(0..0)|string page_file_path) {
      if (!new_pgv || !page_file_path || (page_file_path == "")) {
        //  Split failed so clean up
        new_pgv->delete(1);
        report_error ("Unable to get split layout file for %O.\n", src_page);
        new_pgv = 0;
        return;
      }

      array(REP.Page) dst_pages = new_pgv->pages();
      mapping(string:mixed) src_page_rec =
        src_page->get_rec() &
        ({ "description",
           "ext_source_id" });
      if (REP.Page dst_page = sizeof (dst_pages) && dst_pages[0]) {
        dst_page->set_fields (src_page_rec);
      } else {
        new_pgv->add_pages ( ({ src_page_rec }) );
      }
      new_pgv->store_file_by_copy (base_db_path + page_file_path,
                                   REP.Storage.UNPROC_LAYOUT_FILE,
                                   src_pgv->layout_mime_type(), 0, 0,
                                   // FIXME: Not quite a
                                   // verbatim source..
                                   src_pgv);

      REP.DB.copy_stories_between_page_groups (src_pgv, new_pgv);
      REP.DB.make_pgv_current(new_pgv);
    };

    if (page_file_path == "")
      // Wait for the page extraction to complete. Do this
      // after creating the new pgv to avoid returning the
      // preview for the old one while we wait on this. We'll
      // specify a 10 second timeout to avoid stalling handler
      // threads for too long if there's a huge InDesign
      // backlog. In that case the finish_page_file_copy
      // callback will be called when the operation completes
      // (which in turn will trigger a push notification to
      // clients, if enabled).
      page_file_path =
        src_page->get_split_layout_filepath (3, finish_page_file_copy, 10);

    if (page_file_path) {
      if (sizeof (page_file_path)) {
        // We got the split file already. Otherwise
        // src_page->get_split_layout_filepath will take care
        // to call the callback when it's completed.
        finish_page_file_copy (page_file_path);
      }
    } else {
      new_pgv->delete (1);
      report_error ("Unable to get split layout file for %O.\n", src_page);
      new_pgv = 0;
    }
  }

  return new_pgv;
}

REP.PageSlot copy_page_slot (REP.PageSlot src_slot,
                             void|mapping(string:mixed) field_overrides,
                             void|REP.OnError on_value_error)
//! Creates a new page slot by copying all fields from the given one.
//! The new slot is ordered after the given one. @[field_overrides]
//! may then be given to specify other values for selected fields.
{
  // Note code dup in create_page_slot() and create_page_slots().
  TableLock lock = TableLock (table_locks_for_pgv_and_page_changes);
  Thread.MutexKey ps_lock = page_slot_order_lock();
  return unlocked_copy_page_slot (src_slot, field_overrides, on_value_error);
}

REP.PageSlot unlocked_copy_page_slot (REP.PageSlot src_slot,
                                      void|mapping(string:mixed) fields,
                                      void|REP.OnError on_value_error)
{
  fields =
    (src_slot->get_rec() -
     ({ "page_slot_id", "edition_id", "calc_page_no", "section_id",
        "shadow_parent_id" }) ) +
    (fields || ([]));
  return unlocked_create_page_slot (src_slot->section(), fields,
                                    on_value_error);
}

REP.PageSlot create_empty_padding_slot (REP.PageSlot after_slot,
                                        REP.Edition|REP.Edition.Section ed_or_sec,
                                        int page_order,
                                        int calc_page_no)
//! Creates an empty slot (together with a placeholder page group) to
//! be used for padding to reach a required page position. The slot is
//! inserted after @[after_slot]. If @[after_slot] is zero then the
//! slot is inserted first in @[ed_or_sec].
//!
//! @note
//! Only to be used from the repagination functions in @[REP.Edition]
//! - assumes @[page_slot_order_is_locked].
{
  ASSERT_IF_DEBUG (page_slot_order_is_locked());

  REP.Edition edition =
    ed_or_sec->is_rep_section ? ed_or_sec->edition() : ed_or_sec;
  REP.Edition.Section section =
    ed_or_sec->is_rep_section ? ed_or_sec : edition->main_section();

  ASSERT_IF_DEBUG (edition->id());
  ASSERT_IF_DEBUG (section);

  REP.ObjectTable ps_tbl = page_slots_table();
  REP.PageSlot infant = ps_tbl->create_infant();

  // Could use after_slot as a template here for inheriting properties
  // for the new slot.

  // Since we're called from inside Edition.fix_slot_numbering etc,
  // use the special low-level setter to avoid recursive
  // invalidations.
  infant->internal_update_page_fields ((["edition_id": edition->id(),
                                         "section_id": section->id(),
                                         "page_order": page_order,
                                         "calc_page_no": calc_page_no]));

  REP.PageSlot new_slot = ps_tbl->low_object_create (infant);

  create_placeholder_pgv (new_slot);
  return new_slot;
}

//  // Enable this debug by default.
//  #define DEBUG_SHUFFLE_PAGES_IN_SLOTS
//
//  int(0..1) shuffle_pages_in_slots (
//    mapping(REP.PageSlot:REP.PageSlot) assignments,
//    void|REP.OnError on_move_locked)
//  //! Shuffles around the pages between the page slots in an edition. No
//  //! placeholder pgvs are created or deleted in the process.
//  //!
//  //! For each element in @[assignments], the page currently assigned to
//  //! the @[REP.PageSlot] in the value gets assigned to the page slot in
//  //! the index.
//  //!
//  //! If, after those assignments, there are pages without slots and
//  //! slots without pages then the pages in other slots are shifted,
//  //! without changing their order wrt each other, as necessary to fill
//  //! gaps and make room so that every page ends up in a slot
//  //! afterwards.
//  //!
//  //! Page slots with move locks are not affected. If any page slot
//  //! mentioned in @[assignments] is move locked then it is an error
//  //! that is handled according to @[on_move_locked].
//  //!
//  //! The changes are done in the current pgv's of all affected page
//  //! groups.
//  //!
//  //! @note
//  //! The shuffling only applies to undeleted page slots in the edition.
//  {
//    if (!sizeof (assignments)) return 0;
//
//    // Disallow other REP sessions while performing this
//    // operation. Apparently, various race problems may occur otherwise,
//    // despite table_locks_for_pgv_and_page_changes being held.
//    // Some of the races observed include:
//    //
//    // - Multiple records in the pages table having identical
//    // page_slot_id (allowed by DB design but not supported in higher
//    // level layers nor the GUI.) (Original issue seen in RT #21777.)
//    //
//    // - Page slots missing a corresponding page record (allowed by DB
//    // design but not supported due to missing scratch area -
//    // placeholders should always be present to avoid page listing
//    // breakage).
//    //
//    // - PGVersions being added before page slot shuffling but made
//    // current after shuffling is finished, which will commit old pages
//    // to page slot mappings -- effectively reverting the finished move
//    // operation.
//
//    REP.ExclusiveSessionTracker est = REP.ExclusiveSessionTracker();
//
//  #ifdef DEBUG_SHUFFLE_PAGES_IN_SLOTS
//    ASSERT (sizeof (Array.uniq (values (assignments))) ==
//  	  sizeof (assignments));
//  #endif
//
//    // Need to lock all these tables since
//    // PGVersion.internal_update_pages gets called at the end.
//    TableLock lock = TableLock (table_locks_for_pgv_and_page_changes);
//
//    {
//      mapping(REP.PageSlot:int(1..1)) unassigned_slots = ([]);
//      mapping(REP.PageSlot:int(1..1)) unassigned_pages = ([]);
//
//      REP.Edition ed = get_iterator (assignments)->index()->edition();
//
//      foreach (assignments; REP.PageSlot tgt; REP.PageSlot src) {
//  #ifdef DEBUG_SHUFFLE_PAGES_IN_SLOTS
//        ASSERT (tgt->edition() == ed);
//        ASSERT (src->edition() == ed);
//        ASSERT (!tgt->get ("is_deleted"));
//        ASSERT (!src->get ("is_deleted"));
//  #endif
//        if (tgt->get ("is_move_locked"))
//  	return REP.raise_err (on_move_locked, "Cannot assign page to "
//  			      "move locked page slot %s.\n", tgt->page_name());
//        if (src->get ("is_move_locked"))
//  	return REP.raise_err (on_move_locked, "Cannot assign page from "
//  			      "move locked page slot %s.\n", src->page_name());
//        if (tgt != src) {
//  	if (!m_delete (unassigned_slots, tgt))
//  	  unassigned_pages[tgt] = 1;
//  	if (!m_delete (unassigned_pages, src))
//  	  unassigned_slots[src] = 1;
//        }
//      }
//
//  #ifdef DEBUG_SHUFFLE_PAGES_IN_SLOTS
//      ASSERT (sizeof (unassigned_slots) == sizeof (unassigned_pages));
//      ASSERT (!sizeof (unassigned_slots & unassigned_pages));
//  #endif
//
//      if (sizeof (unassigned_slots)) {
//        array(REP.PageSlot) slots = ed->page_slots();
//        array(REP.PageSlot) back_queue = ({}), forw_queue = ({});
//
//        foreach (slots; int i; REP.PageSlot slot)
//  	if (!slot->get ("is_deleted")) {
//  	  int feed, drain;
//  	  if (unassigned_pages[slot])
//  	    feed = 1;
//  	  else if (unassigned_slots[slot])
//  	    drain = 1;
//  	  else if (!assignments[slot] &&
//  		   (sizeof (back_queue) || sizeof (forw_queue)) &&
//  		   !slot->get ("is_move_locked"))
//  	    feed = drain = 1;
//
//  	  if (feed) {
//  	    if (sizeof (back_queue)) {
//  	      assignments[back_queue[0]] = slot;
//  	      back_queue = back_queue[1..];
//  	    }
//  	    else
//  	      forw_queue += ({slot});
//  	  }
//
//  	  if (drain) {
//  	    if (sizeof (forw_queue)) {
//  	      assignments[slot] = forw_queue[0];
//  	      forw_queue = forw_queue[1..];
//  	    }
//  	    else
//  	      back_queue += ({slot});
//  	  }
//  	}
//
//  #ifdef DEBUG_SHUFFLE_PAGES_IN_SLOTS
//        ASSERT_IF_DEBUG (!sizeof (back_queue /*%O*/), back_queue);
//        ASSERT_IF_DEBUG (!sizeof (forw_queue /*%O*/), forw_queue);
//  #endif
//      }
//
//  #ifdef DEBUG_SHUFFLE_PAGES_IN_SLOTS
//      // Sanity check that every involved slot both gets rid of its old
//      // page and gets a new one.
//      mapping(REP.PageSlot:int) check = ([]);
//      foreach (assignments; REP.PageSlot tgt; REP.PageSlot src) {
//        check[tgt] |= 1;
//        check[src] |= 2;
//      }
//      ASSERT (!sizeof (values (check /*%O*/) - ({3}))
//  	    /*assignments: %O*/, check, assignments);
//  #endif
//    }
//
//    REP.CachedTable pages_tbl = pages_table();
//    mapping(REP.PGVersion:REP.DBObject.DisableAutoCommit) page_group_dacs = ([]);
//    mapping(REP.Page:REP.PageSlot) page_assignments = ([]);
//
//    foreach (assignments; REP.PageSlot tgt; REP.PageSlot src)
//      if (REP.Page page = src->low_get_page()) {
//        page_assignments[page] = tgt;
//        REP.PGVersion pgv = page->pgv();
//        if (!page_group_dacs[pgv])
//  	page_group_dacs[pgv] = pgv->DisableAutoCommit();
//      }
//
//    // Note that the page slot assignments are not logically part of the
//    // versioned info in the PGVersions (even though they currently get
//    // stored in old pgv's). Therefore we don't need to add new pgv's
//    // here.
//
//    foreach (page_assignments; REP.Page page; REP.PageSlot new_slot)
//      page->set_fields ((["page_slot": new_slot]));
//
//    // Update the table directly before the page groups are committed,
//    // so that PGVersion.internal_update_pages won't notice the change
//    // and start juggling with placeholders.
//    foreach (page_assignments; REP.Page page; REP.PageSlot new_slot) {
//  #ifdef DEBUG_SHUFFLE_PAGES_IN_SLOTS
//      ASSERT (page->pgv() == page->pgv()->current_pgv());
//  #endif
//      pages_tbl->cached_update ((["page_id": page->id(),
//  				"page_slot_id": new_slot->id()]));
//    }
//
//    // Explicit destructs for explicitness sake..
//    foreach (page_group_dacs;; REP.DBObject.DisableAutoCommit dac)
//      destruct (dac);
//    destruct (lock);
//
//    destruct (est);
//
//    return 1;
//  }

int(0..1) reorder_page_slots (array(REP.PageSlot) slot_order,
                              void|REP.OnError on_move_locked)
//! Rearranges the given page slots so that they get in the given
//! order relative to each other. If there are more page slots between
//! those then they end up afterwards, except those with move locks
//! which stay where they are.
//!
//! Tries to keep together "sibling" slots, i.e. those that share
//! page_order number: If siblings occur next to each other in page
//! order in @[slot_order] then they are kept siblings, and if there
//! are siblings among other slots not included in @[slot_order], they
//! are kept siblings as well.
//!
//! If any page slot in @[slot_order] except the first is move locked,
//! or any sibling of them, then it is an error that is handled
//! according to @[on_move_locked].
{
  if (!sizeof (slot_order)) return 1;

  ASSERT_IF_DEBUG (sizeof (Array.uniq (slot_order)) == sizeof (slot_order));
  ASSERT_IF_DEBUG (sizeof (Array.uniq (slot_order->section()->id())) == 1);

  REP.PageSlot first = slot_order[0];

  // Save the siblings for each slot. Let's also keep track of deleted ones.
  mapping(REP.PageSlot:array(REP.PageSlot)) siblings =
    mkmapping (slot_order, slot_order->siblings (1));

  // First remove siblings that should be kept siblings. It's enough
  // to keep one of them and let the sibling handling below update the
  // other ones.
  array(REP.PageSlot) later = ({});
  REP.PageSlot prev_slot = first;
  int prev_po = prev_slot->get ("page_order");

  for (int i = 1; i < sizeof (slot_order); i++) {
    REP.PageSlot slot = slot_order[i];
    ASSERT_IF_DEBUG (slot->edition() == first->edition());
    ASSERT_IF_DEBUG (!slot->get ("is_deleted"));

    foreach (({slot}) + siblings[slot], REP.PageSlot check_slot)
      if (check_slot->get ("is_move_locked") &&
          !check_slot->get ("is_deleted"))
        return REP.raise_err (on_move_locked, "Attempt to move page slot %s "
                              "which is move locked.\n",
                              check_slot->page_name());

    int po = slot->get ("page_order");
    if (po != prev_po || slot < prev_slot) {
      later += ({slot});
      prev_slot = slot;
      prev_po = po;
    }
  }

  TableLock lock = TableLock (table_locks_for_page_slot_renumber);
  REP.Edition.DelayPageSlotRenumber dpsr =
    first->edition()->DelayPageSlotRenumber();

  // Now reorder each slot after the previous one, and let the
  // siblings tag along.
  prev_slot = first;
  foreach (later, REP.PageSlot slot) {
    slot->set_fields ((["after_slot": prev_slot]));
    int po = slot->get ("page_order");
    foreach (siblings[slot], REP.PageSlot sibling)
      // May have deleted move locked slots here. Just ignore them.
      if (!sibling->get ("is_move_locked"))
        sibling->set_fields ((["page_order": po]));
    prev_slot = slot;
  }

  destruct (dpsr);
  destruct (lock);

  return 1;
}

array(REP.PageSlot) sort_page_slots_by_order (array(REP.PageSlot) slots)
//! Returns an array that contains the given page slots sorted by
//! page_order.
//!
//! @seealso
//! @[sort_pages_by_page_slot_order]
{
  ASSERT_IF_DEBUG (!sizeof (slots) ||
                   !sizeof (slots->edition() - ({slots[0]->edition()})));
  array(int) slots_order = slots->get ("page_order");
  slots += ({});
  sort (slots_order, slots);
  return slots;
}


array(mapping) get_edition_page_groups_info(REP.Edition ed)
{
  // Iterate over the page groups in page slot order.
  array(mapping(string:mixed)) pg_recs = ({ });
  mapping(REP.PGVersion:int(1..1)) added = ([ ]);
  foreach (ed->page_slots(0), REP.PageSlot slot) {
    REP.PGVersion pgv = slot->page()->pgv();
    if (!added[pgv]) {
      // Misses in the added mapping are possible due to races
      // where a new pgv has become current, but that's no biggie.
      added[pgv] = 1;
      array(REP.PageSlot) slots = pgv->pages()->page_slot() - ({ 0 });
      pg_recs += ({
          ([ "page_group_id": pgv->get("page_group_id"),
             "group_name": REP.Utils.group_page_nums(slots->page_name()),
             "section_id": sizeof(slots) && slots[0]->section()->id() ])
        });
    }
  }

  return pg_recs;
}

//! Copies all @[REP.PGVersion]s and all @[REP.PageSlot]s they are
//! associated to. Page slots will be appended to @[dst_edition].
//!
//! @param src_edition
//!   The source @[REP.Edition].
//! @param dst_edition
//!   The destination @[REP.Edition].
//!
//! @returns
//! Returns true if the copy completed successfully. In case of an error
//! there might still be a partial copy performed.
Concurrent.Future/*array(REP.PGVersion)*/
copy_edition_contents(REP.Edition src_edition, REP.Edition dst_edition,
                      void|bool create_shadow_slots)
{
  //  Strategy to handle various combinations of sections in the source and
  //  destination editions. The approach here is also exposed in the UI in
  //  the method copyAllPages() in page-management2.js where the user is
  //  informed through a confirmation prompt before the action starts.
  //
  //    A) No sections in either source or destination
  //
  //         Straightforward where pages are copied without any special
  //         consideration.
  //
  //
  //    B) Sections in source, no sections in destination
  //
  //         The destination will be upgraded to use the same section
  //         definitions as the source. Definitions are not exclusive to
  //         any publication so there shouldn't be any conflicts in doing
  //         that. Any existing pages will be moved to the main section of
  //         the new section-based page plan.
  //
  //         Applying the new section config will not include any source
  //         sections that have weekday schedules that don't apply to the
  //         destination's publication date. This can happen when the source
  //         is a template edition that can carry sections for all weekdays
  //         in parallel.
  //
  //         After sections have been created the pages are copied into
  //         each section.
  //
  //
  //    C) Sections in both source and destination
  //
  //         We can't assign the same section setup as the source uses since
  //         that would potentially remove existing sections. Instead we
  //         only add missing sections (determined by the "ref" field).
  //         Again any section with a weekday schedule in the source that
  //         isn't applicable to the destination's publication date will be
  //         filtered out.
  //
  //         Following section creation the pages are copied into each
  //         section.
  //
  //  As of this writing sections have no additional properties that will
  //  be carried over, but a future version may include user properties,
  //  deadlines or similar configuration that should be handled as well.

  array(REP.Edition.Section) src_sections =
    !src_edition->has_fallback_section() && src_edition->all_sections();
  array(REP.Edition.Section) dst_sections =
    !dst_edition->has_fallback_section() && dst_edition->all_sections();

  REP.Notification.PushContext push_context =
    REP.Notification.EditionPushContext (
      REP.Types.ClientMessages.page_list,
      dst_edition);

  REP.Notification.Client.InhibitBroadcast ib =
    REP.Notification.Client.inhibit_broadcast (push_context);

  string db_root = REP.get_print_module()->get_file_db_path();
  array(Concurrent.Future/*array(REP.PGVersion)*/) futures = ({ });
  int fail;

  Concurrent.Future/*array(REP.PGVersion)*/
  low_copy_section(REP.Edition.Section src_section,
                   REP.Edition.Section dst_section) {
    //  Take the lock separately for each section
    TableLock lock = TableLock (table_locks_for_page_slot_renumber);
    REP.Edition.DelayPageSlotRenumber dpsr =
      dst_edition->DelayPageSlotRenumber();

    REP.PageSlot prev_slot;
    array(REP.PageSlot) existing_dst_slots = dst_section->page_slots();
    if (sizeof (existing_dst_slots)) {
      prev_slot = existing_dst_slots[-1];
    }

    array(REP.PageSlot) dst_slots = ({});
    array(REP.PageSlot) src_slots = src_section->page_slots();

    foreach (src_slots, REP.PageSlot src_slot) {
      mapping create_fields = ([
        "subspec": src_slot->get ("subspec"),
        "after_slot": prev_slot
      ]);
      if (int fixed_page_no = src_slot->get ("fixed_page_no"))
        create_fields["fixed_page_no"] = fixed_page_no;
      if (mixed page_color = src_slot->get("page_color"))
        create_fields["page_color"] = page_color;

      REP.PageSlot new_slot = create_page_slot (dst_section, create_fields);

      //  Link slot if caller wants shadows instead of real copies
      if (create_shadow_slots)
        new_slot->link_shadow(src_slot);

      prev_slot = new_slot;
      dst_slots += ({ new_slot });
    }

    destruct (dpsr);
    destruct (lock);

    Concurrent.Future/*array(REP.PGVersion)*/ low_copy_future;
    if (create_shadow_slots) {
      //  Story appearances in the source edition should be extended to
      //  include shadow slots as well as the stack of the new destination.
      //  We can exclude placed stories since they will get appearances
      //  through deconstruct.
      mapping(int:array(REP.Story)) stories_by_ps_id = ([ ]);
      array(REP.Story) src_stories = list_stories(src_edition);
      foreach (src_stories, REP.Story src_story) {
        mapping(int:mapping(string:mixed)) sa_parts =
          src_story->story_app_parts();
        foreach (sa_parts; int sa_part_id; mapping app) {
          if (!app->is_placed) {
            //  If not stack appearance, map source page group to page slots
            array(int) ps_ids = ({ });
            if (int pg_id = app->page_group_id) {
              if (REP.PGVersion src_pgv =
                  REP.DB.current_pgv_by_pg_id(pg_id, REP.RETURN_ZERO))
                ps_ids = (src_pgv->pages()->page_slot() - ({ 0 }) )->id();
            } else {
              //  Stack appearance
              ps_ids = ({ 0 });
            }
            foreach (ps_ids, int ps_id)
              stories_by_ps_id[ps_id] += ({ src_story });
          }
        }
      }
      foreach ( ({ 0 }) + dst_slots, REP.PageSlot dst_slot) {
        if (!dst_slot) {
          //  Stack appearances
          foreach (stories_by_ps_id[0] || ({ }), REP.Story s)
            REP.DB.add_story_app(s, dst_edition, 0, 0);
        } else if (REP.PageSlot parent_ps = dst_slot->shadow_parent())
          if (array(REP.Story) map_stories = stories_by_ps_id[parent_ps->id()])
            foreach (map_stories, REP.Story s) {
              //  Regular appearances
              if (REP.PGVersion dst_pgv = dst_slot->page()->pgv())
                REP.DB.add_story_app(s, dst_edition, dst_pgv, 0);
            }
      }

      //  We trigger targeted shadow slot updates in our new section only.
      //  Pick a shadow copy policy that reflects whether we're branching a
      //  zone or time-based edition.
      ShadowCopyPolicy shadow_copy_policy;
      if (src_edition->ze_zone_code() == dst_edition->ze_zone_code()) {
        //  Same zone means different time-based edition. In this case we
        //  won't have any unique ad info in the destination edition so we
        //  copy all layers (editorial and ads) into our shadow.
        shadow_copy_policy = OVERWRITE_ALL;
      } else {
        //  Different zones so policy depends on if ads are managed per zone
        //  or duplicated into every zone. We can defer this decision by
        //  using the default policy.
        shadow_copy_policy = DEFAULT_SHADOW_POLICY;
      }

      low_copy_future =
        queue_update_shadow_pgvs(src_edition->current_pg_versions(src_section),
                                 ([ dst_edition: shadow_copy_policy ]) );
    } else {
      int now = time();
      array(REP.PGVersion) new_pgvs = ({ });
      foreach (src_edition->current_pg_versions(src_section),
               REP.PGVersion src_pgv) {
        if (src_pgv->get_unver ("is_deleted")) continue;

        array(REP.Page) src_pages = src_pgv->pages();

        if (!sizeof (filter (src_pages,
                             lambda (REP.Page p)
                             { REP.PageSlot slot = p->page_slot();
                               return slot && !slot->get ("is_deleted");
                             })))
          continue; // If no page in the page group is connected to a
        // non-deleted slot, skip the entire page group.

        array(REP.PageSlot) src_pgv_pss = src_pages->page_slot() - ({ 0 });
        string new_pgv_comment =
          "Copied from " + (src_edition->get("title") || "nameless issue") +
          ", page " + (src_pgv_pss->page_name() * ", ");
        REP.PGVersion new_pgv =
          copy_page_group_fields (src_pgv, dst_edition, 0,
                                  ([ "date_created": now,
                                     "comment": new_pgv_comment ]),
                                  REP.LOG_ERROR);

        if (!new_pgv) {
          fail = 1;
          continue; // Try to copy as much as we can, even if something fails.
        }

        array(mapping(string:mixed)) new_page_recs =
          map (src_pgv->pages(),
               lambda (REP.Page page) {
                 return page->get_rec() - ({"page_id", "page_slot_id"});
               });
        new_pgv->add_pages (new_page_recs);

        array(REP.Page) new_pages = new_pgv->pages();

        foreach (src_pages; int page_index; REP.Page src_page) {
          REP.Page dst_page = new_pages[page_index];

          int ps_index;
          REP.PageSlot src_slot;
          if ((src_slot = src_page->page_slot()) &&
              ((ps_index = search (src_slots, src_slot)) != -1)) {
            REP.PageSlot new_slot = dst_slots[ps_index];
            dst_page->set_fields (([ "page_slot" : new_slot ]));
          } else {
            // We can't leave the page unassigned since the InDesign
            // module would recreate a new slot for it when it does its
            // processing, so we'll create a deleted slot to put it in.
            REP.PageSlot deleted_slot =
              create_page_slot (dst_edition, ([ "is_deleted" : 1 ]));
            dst_page->set_fields (([ "page_slot" : deleted_slot ]));
          }
        }

        copy_stories_between_page_groups (src_pgv, new_pgv);

        if (string src_path =
            src_pgv->get_existing_layout_filepath (REP.RETURN_ZERO))
          new_pgv->store_file_by_copy (db_root + src_path,
                                       REP.Storage.UNPROC_LAYOUT_FILE,
                                       src_pgv->layout_mime_type(),
                                       UNDEFINED,
                                       UNDEFINED,
                                       src_pgv);

        make_pgv_current (new_pgv);
        new_pgvs += ({ new_pgv });
      }

      //  Join all single PGV result. We won't wait for the new PGVs to be
      //  previewed so this PGV might not be fully initialized with all
      //  REP.Page objects etc.
      low_copy_future = Concurrent.resolve(new_pgvs);
    }

    return low_copy_future;
  };

  if (!src_sections && !dst_sections) {
    //  Case A -- No sections in either source or destination
    Concurrent.Future/*array(REP.PGVersion)*/ f =
      low_copy_section(src_edition->main_section(),
                       dst_edition->main_section());
    futures += ({ f });
  } else if (src_sections && !dst_sections) {
    //  Case B -- Sections in source, no sections in destination

    //  Apply filtered sections to destination edition. This will move all
    //  existing pages into the main section.
    mapping(int|string:mapping) src_section_defs_raw =
      src_edition->get("section_defs") + ([ ]);
    int apply_ok = dst_edition->apply_section_defs(src_section_defs_raw);
    if (apply_ok) {
      //  Loop over relevant sections. We match them on the "ref" field.
      foreach (src_sections, REP.Edition.Section src_section) {
        string src_section_ref = src_section->def()->ref();
        if (REP.Edition.Section dst_section =
            dst_edition->get_section_by_ref(src_section_ref)) {
          Concurrent.Future/*array(REP.PGVersion)*/ f =
            low_copy_section(src_section, dst_section);
          futures += ({ f });
        }
      }
    } else {
      fail = 1;
    }
  } else {
    //  Case C -- Sections in both source and destination

    //  Get filtered source sections and see which ones that need to be added
    //  to the destination. Any additions will take place at the end.
    int(0..0)|mapping(int:mapping) src_section_defs_norm =
      dst_edition->normalize_section_defs(src_edition->get("section_defs"));
    if (src_section_defs_norm) {
      array(mapping) copy_defs_raw = ({ });
      mapping(string:int) src_section_ids = ([ ]);
      foreach (src_section_defs_norm; int section_id; mapping src_def) {
        //  If corresponding section is missing from destination we queue
        //  it for copying.
        if (!dst_edition->get_section_by_ref(src_def->ref)) {
          copy_defs_raw += ({ src_def });
          src_section_ids[src_def->ref] = section_id;
        }
      }

      int apply_ok = 1;
      if (sizeof(copy_defs_raw)) {
        //  Update the sort order number for the new entries
        int dst_sort_order_base = dst_sections[-1]->sort_order() + 1;

        //  Find the next free section ID to use. We'll try to keep the IDs
        //  used in the source if they are unused in the destination.
        int next_dst_section_id = max(max(@dst_sections->id()) + 1, 100);

        mapping(int|string:mapping) dst_section_defs_raw =
          dst_edition->get("section_defs") + ([ ]);
        for (int i = 0; i < sizeof(copy_defs_raw); i++) {
          mapping src_def_raw = copy_defs_raw[i];
          int dst_section_id = src_section_ids[src_def_raw->ref];
          if (dst_section_defs_raw[dst_section_id])
            dst_section_id = next_dst_section_id++;
          dst_section_defs_raw[dst_section_id] =
            src_def_raw + ([ "sort_order": dst_sort_order_base + i ]);
        }

        apply_ok = dst_edition->apply_section_defs(dst_section_defs_raw);
      }

      if (apply_ok) {
        //  Now loop over relevant sections. We match them on the "ref" field.
        foreach (src_sections, REP.Edition.Section src_section) {
          REP.Edition.SectionDef src_section_def = src_section->def();
          string src_section_ref = src_section_def->ref();
          if (REP.Edition.Section dst_section =
              dst_edition->get_section_by_ref(src_section_ref)) {
            Concurrent.Future/*array(REP.PGVersion)*/ f =
              low_copy_section(src_section, dst_section);
            futures += ({ f });
          }
        }
      } else {
        fail = 1;
      }
    }
  }

  destruct (ib);

  return !fail && Concurrent.results(futures);
}



// Editions

array(REP.Edition) editions_in_publication(REP.Publication pub,
                                           void|int(0..1) templates_only,
                                           void|int(0..1) include_deleted)
{
  return editions_table()->object_select("publication_id = " + pub->id() +
                                         (templates_only ?
                                          " AND is_template = 1" :
                                          "") +
                                         (!include_deleted ?
                                          " AND is_deleted = 0" :
                                          ""));
}

array(int) template_edition_ids_in_publication(REP.Publication pub)
{
  //  Since only a handful will be template issues we cache them to avoid
  //  a costly enumeration of all editions. An issue can never gain/lose
  //  template flag once it's been created so the cache only needs to be
  //  cleaned when a new edition is created.
  REP.PrintDBModule print_db = REP.get_print_module();
  array(int) res =
    print_db->all_tmpl_editions_cache &&
    print_db->all_tmpl_editions_cache[pub->id()];
  if (!res) {
    //  Rebuild cache
    array(REP.Edition) tmpl_editions =
      REP.DB.editions_in_publication(pub, 1, 1);
    if (!print_db->all_tmpl_editions_cache)
      print_db->all_tmpl_editions_cache = ([ ]);
    res = print_db->all_tmpl_editions_cache[pub->id()] = tmpl_editions->id();
  }
  return res;
}

REP.Edition edition_by_id(int(1..) edition_id,
                          void|REP.OnError on_rec_not_found) {
  return editions_table()->object_get (edition_id, on_rec_not_found);
}

REP.Edition edition_by_uuid(string edition_uuid,
                          void|REP.OnError on_rec_not_found) {
  REP.ObjectTable tbl = editions_table();
  REP.Edition edition = edition_by_id (rec_id_by_uuid (tbl, edition_uuid),
                                       REP.RETURN_ZERO);

  if (edition)
    return edition;

  return REP.raise_err(on_rec_not_found,
                       "Edition with uuid %s not found.\n",
                       edition_uuid);
}

REP.Edition
create_edition(int|REP.Publication pub,
               int|string|mapping(string:int)|Calendar.Day publ_date,
               void|string title,
               void|int planned_num_pages,
               void|int(0..1) is_template,
               void|int(0..1) skip_sections,
               void|REP.OnError on_value_error)
//! Create a new edition for a publication.
//!
//! @returns
//!       The new publication object.
{

  if (!objectp(pub)) {
    pub = publication_by_id(pub, on_value_error);
    if (!pub) return 0;
  }

  mapping(string:mixed) rec = ([
    "publ_date" : publ_date,
    "title"     : title,
    "planned_num_pages" : 0,  //  Unused?
    "publication" : pub,
    "is_template": is_template,
  ]);
  REP.Edition ed = editions_table()->object_create(rec, on_value_error);

  if (ed) {
    if (is_template) {
      //  Sync template edition lookup cache. Note that the cache doesn't
      //  need invalidation for regular editions.
      REP.PrintDBModule print_db = REP.get_print_module();
      if (mapping(int:array(int)) tmpl_cache =
          print_db->all_tmpl_editions_cache) {
        if (tmpl_cache[pub->id()])
          tmpl_cache[pub->id()] += ({ ed->id() });
      }
    }

    mapping update_fields = ([ ]);

    //  Collect section definitions and "flatten" them to match the target
    //  date of this edition (if not a template edition).
    if (!skip_sections) {
      if (mapping(int:mapping) section_defs = pub->flattened_section_defs(ed))
        update_fields += ([ "section_defs": section_defs ]);
    }

    //  If the parent publication defines a zoning/editioning config we
    //  flag this edition as a master edition. This happens regardless of
    //  past or future publication date, but in case of the former the
    //  edition will archive a snapshot of the definitions immediately.
    if (!is_template) {
      if (array(mapping) pub_ze_defs = pub->get_ze_defs()) {
        //  Get the first zone code and optionally the edition code
        mapping master_def = pub_ze_defs[0];
        update_fields += ([
          "ze_master_edition_id": ed->id(),
          "ze_zone_code": master_def->zone_code,
          "ze_edition_code": (sizeof(master_def->editions || ({ }) ) &&
                              master_def->editions[0]->edition_code)
        ]);
      }
    }

    if (sizeof(update_fields))
      ed->set_fields(update_fields);
  }

  return ed;
}


string|Concurrent.Future/*REP.Edition*/
create_edition_ze(void|REP.Publication pub,
                  void|REP.Edition parent_ed,
                  void|int|string|mapping(string:int)|Calendar.Day publ_date,
                  void|string new_zone_code,
                  void|string new_edition_code,
                  bool copy_sections,
                  bool link_pages,
                  void|REP.OnError on_value_error)
{
  //  This is the zone/edition-aware implementation of create_edition().
  //  If this is a branch from a known parent edition we accept that as
  //  input (together with Z/E info for the branch), but passing
  //  publication, date and Z/E info will also resolve an existing parent/
  //  master edition, or create a new one if missing.
  //
  //  Branching from an edition that isn't yet flagged as a Z/E master will
  //  force its master status to be set.
  //
  //  The branch's publication date need not follow the parent/master, but
  //  it's considered good form to make them equal. If different the UI
  //  will show the special title, but Edit Edition and other dialog boxes
  //  typically resets both the date and title of all editions in the
  //  collection.
  //
  //  The flags for copy sections and link pages control if the new edition
  //  is totally empty, empty with pre-defined sections, or populated with
  //  sections as well as shadowed page slots from its parent. It's not
  //  acceptable to enable page linking without also copying sections (even
  //  when the parent/master is section-less).

  //  We must be able to resolve publication or parent edition
  if (!pub && !parent_ed)
    return REP.raise_err(on_value_error,
                         "Missing publication and parent edition.");
  if (!pub)
    pub = parent_ed->publication();

  if (pub && !parent_ed) {
    //  Without a known parent a publication date is necessary. Normalize
    //  the wide spectrum of supported formats to a type supported by
    //  editions_by_date() below.
    if (!publ_date)
      return REP.raise_err(on_value_error, "Missing publication date.");
    if (mappingp(publ_date)) {
      //  Sanity check date parts
      mapping(string:int) t = ([ "year": publ_date->year - 1900,
                                 "mon": publ_date->month - 1,
                                 "mday": publ_date->day ]);
      if (!equal(t & localtime(mktime(t)), t))
        return REP.raise_err(on_value_error,
                             "Incorrect publication date.");
      publ_date =
        sprintf("%04d%02d%02d",
                publ_date->year, publ_date->month, publ_date->day);
    } else if (intp(publ_date)) {
      if (publ_date > 20000000)
        publ_date = (string) publ_date;
    }

    //  Try to locate a parent edition based on publication date.
    //
    //  If there are multiple candidates we try to find one that already
    //  acts as a master edition for extending that collection. If only
    //  one candidate is present we take that and accept we may need to
    //  promote it to a master edition (unless we ourselves are destined
    //  to become a master, but that's checked later below).
    array(REP.Edition) parent_candidates =
      pub->editions_by_date(publ_date);
    if (sizeof(parent_candidates) == 1) {
      parent_ed = parent_candidates[0];
    } else {
      parent_candidates =
        filter(parent_candidates, lambda(REP.Edition e) {
            return e->is_ze_master_edition();
          });
      if (sizeof(parent_candidates) == 1) {
        parent_ed = parent_candidates[0];
      }
    }
  }

  //  A deleted parent edition isn't eligible. The caller should ensure its
  //  validity before calling.
  if (parent_ed && parent_ed->get("is_deleted"))
    return REP.raise_err(on_value_error,
                         "Cannot branch from deleted parent edition.");

  //  Fetch the Z/E definitions we currently follow. They may be archived
  //  in the parent edition.
  array(mapping) ze_defs = (parent_ed || pub)->get_ze_defs();
  if (!ze_defs || !sizeof(ze_defs)) {
    //  Not finding any Z/E definition is a blocker if we're given a
    //  specific zone and/or edition code and/or branch parent. For
    //  remaining scenarios we can just call create_edition().
    if (parent_ed || new_zone_code || new_edition_code || !publ_date)
      return REP.raise_err(on_value_error,
                           "No zoning/editioning configuration found.");
    REP.Edition ed = create_edition(pub, publ_date, 0, 0, 0, 0, on_value_error);
    return Concurrent.resolve(ed);
  }

  //  Is the new edition aspiring to be a master of its own Z/E collection?
  //  Missing zone/edition codes is a sign of that, but such call cannot be
  //  combined with an existing parent.
  string master_zone_code = ze_defs[0]->zone_code;
  string master_edition_code =
    ze_defs[0]->editions && ze_defs[0]->editions[0]->edition_code;
  if (!new_zone_code)
    new_zone_code = master_zone_code;
  if (!new_edition_code && (new_zone_code == master_zone_code))
    new_edition_code = master_edition_code;
  bool wants_to_be_master =
    (new_zone_code == master_zone_code) &&
    (new_edition_code == master_edition_code);
  if (wants_to_be_master && parent_ed)
    return REP.raise_err(on_value_error,
                         "Conflicting main editions.");

  //  Conversely, requesting a non-master zone/edition without having
  //  found a parent is also a no-go.
  if (!wants_to_be_master && (new_zone_code || new_edition_code) &&
      !parent_ed)
    return REP.raise_err(on_value_error, "No main edition found.");

  ASSERT_IF_DEBUG(!!wants_to_be_master ^ !!parent_ed);

  //  Verify valid zone/edition combination. Lookup table initlally only
  //  holds a flag for existing keys, but later we'll upgrade entries to
  //  edition references when we look for collisions.
  mapping(string:REP.Edition|int(1..1)) ze_lookup = ([ ]);
  foreach (ze_defs, mapping ze_def) {
    if (ze_def->editions) {
      foreach (ze_def->editions, mapping e) {
        string ze_key = sprintf("Z:%s|E:%s",
                                lower_case(ze_def->zone_code),
                                lower_case(e->edition_code));
        ze_lookup[ze_key] = 1;
      }
    } else {
      string ze_key = sprintf("Z:%s|E:", lower_case(ze_def->zone_code));
      ze_lookup[ze_key] = 1;
    }
  }
  string new_ze_key =
    sprintf("Z:%s|E:%s",
            lower_case(new_zone_code),
            lower_case(new_edition_code || ""));
  if (!ze_lookup[new_ze_key])
    return REP.raise_err(on_value_error,
                         "Requested zone code and/or edition code not "
                         "available.");

  //  If a parent is known and it has Z/E properties we get the existing
  //  collection and look for collisions.
  if (parent_ed && parent_ed->is_ze_edition()) {
    array(REP.Edition) ze_eds = parent_ed->ze_collection();
    foreach (ze_eds, REP.Edition ze_ed) {
      string ze_key = sprintf("Z:%s|E:%s",
                              lower_case(ze_ed->ze_zone_code()),
                              lower_case(ze_ed->ze_edition_code() || ""));
      ze_lookup[ze_key] = ze_ed;
    }
    if (objectp(ze_lookup[new_ze_key]))
      return REP.raise_err(on_value_error,
                           "Requested zone code and/or edition code already "
                           "in use.");
  } else if (parent_ed) {
    //  We need to promote parent to a collection master so we can branch
    //  from it.
    parent_ed->set_fields( ([ "ze_master_edition_id": parent_ed->id(),
                              "ze_zone_code": master_zone_code,
                              "ze_edition_code": master_edition_code ]) );
  }

  //  Update publication date if we didn't get a value so far
  if (!publ_date && parent_ed)
    publ_date = parent_ed->get("publ_date");

  //  Now create the branch, flag it as master if necessary, and assign
  //  Z/E properties. We don't apply standard section definitions but instead
  //  use parent sections if available.
  //
  //  We also reuse parent title in case it has been customized; this is not
  //  perfect if the dates differ (and the date is embedded in the title),
  //  but that is not a typical use-case.
  bool skip_default_sections = !copy_sections || !!parent_ed;
  string parent_title = parent_ed && parent_ed->get("title");
  REP.Edition new_ed =
    create_edition(pub, publ_date, parent_title, 0, 0, skip_default_sections,
                   on_value_error);
  if (new_ed) {
    //  We either use the collection's existing master, or we are the new
    //  master ourselves.
    int master_id =
      parent_ed ? parent_ed->get("ze_master_edition_id") : new_ed->id();
    mapping(string:mixed) new_fields = ([
      "ze_parent_edition_id": parent_ed && parent_ed->id(),
      "ze_master_edition_id": master_id,
      "ze_zone_code": new_zone_code,
      "ze_edition_code": new_edition_code
    ]);

    //  Apply parent sections if we skipped publication defaults earlier.
    //  However, if publication dates differ we cannot reuse them reliably
    //  since they might include a weekly schedule that flattens to a
    //  different result.
    if (skip_default_sections && copy_sections) {
      bool same_date = parent_ed->get("publ_date") == new_ed->get("publ_date");
      mapping(int:mapping) new_section_defs =
        same_date ?
        parent_ed->get("section_defs") :
        pub->flattened_section_defs(new_ed);
      if (new_section_defs)
        new_fields += ([ "section_defs": new_section_defs ]);
    }

    new_ed->set_fields(new_fields);

    //  Link pages
    if (parent_ed && copy_sections && link_pages) {
      Concurrent.Future/*array(REP.PGVersion)*/ f =
        copy_edition_contents(parent_ed, new_ed, true);
      return f->then(lambda(array(REP.PGVersion) new_pgvs) {
          return new_ed;
        });
    }
  }
  return Concurrent.resolve(new_ed);
}



// User properties

//! Find the definitions for all user properties that are required
//! to be defined at the specified @[level].
//!
//! Returns the definitions for all user properties for the publication
//! if no @[level] is specified.
array(REP.PropDef) propdefs_by_prefix_and_level(string prefix,
                                                string|void level)
{
  array(REP.PropDef) base = ({});
  if (prefix != "") {
    base = propdefs_by_prefix_and_level("", level);
  }

  //  Check cache first, but beware of any weak reference that has been
  //  converted to zero. We rely on the interpreter lock to access/update
  //  the cache.
  REP.PrintDBModule print_db = REP.get_print_module();
  string cache_key = "prefixlevels:" + prefix + "|" + (level || "0");
  array(REP.PropDef) res, cached_res;
  if ((cached_res = print_db->user_propdefs_cache[cache_key]) &&
      !has_value(cached_res, 0)) {
    res = cached_res + ({ });
  } else {
    REP.ObjectTable tbl = user_property_defs_table();
    string query = "publication_prefix = '" + tbl->quote(prefix) + "'";
    if (level) {
      query += " AND required_at = '" + tbl->quote(level) + "'";
    }
    res = tbl->object_select(query);
    print_db->user_propdefs_cache[cache_key] = res + ({ });
  }

  if (sizeof(base)) {
    if (!sizeof(res)) return base;

    // We need to join the two...
    mapping(string:int) seen = ([]);
    foreach(res, REP.PropDef pd) {
      seen[pd->get("property_id")] = 1;
    }
    foreach(base, REP.PropDef pd) {
      if (seen[pd->get("property_id")]) continue;
      res += ({ pd });
    }
  }
  return res;
}

//! Get the definition for a specific user property given its
//! publication prefix and property name.
REP.PropDef propdef_by_prefix_and_name(string prefix,
                                       string propname,
                                       void|REP.OnError on_rec_not_found)
{
  //  Check cache first, but beware of any weak reference that has been
  //  converted to zero. We rely on the interpreter lock to access/update
  //  the cache.
  array(REP.PropDef) res, cached_res;
  REP.PrintDBModule print_db = REP.get_print_module();
  string cache_key = "prefixname:" + prefix + "|" + propname;
  if ((cached_res = print_db->user_propdefs_cache[cache_key]) &&
      !has_value(cached_res, 0)) {
    res = cached_res + ({ });
  } else {
    REP.ObjectTable tbl = user_property_defs_table();
    res =
      tbl->object_select("publication_prefix = '" + tbl->quote(prefix) + "'"
                         " AND property_id = '" + tbl->quote(propname) + "'");
    print_db->user_propdefs_cache[cache_key] = res + ({ });
  }

  if (sizeof(res)) return res[0];
  if (prefix != "") {
    return propdef_by_prefix_and_name("", propname, on_rec_not_found);
  }
  REP.raise_err(on_rec_not_found,
                "No such property: %s.\n", propname);
}

//! Get a property definition given its internal id.
REP.PropDef propdef_by_id(int id, void|REP.OnError on_rec_not_found)
{
  return user_property_defs_table()->object_get(id, on_rec_not_found);
}

REP.PropDef create_user_prop_def(string pub_prefix,
                                 string user_prop,
                                 mapping(string:mixed) def,
                                 void|REP.OnError on_error)
{
  //  Default value will be added after object is created
  REP.Publication pub =
    (pub_prefix != "") && publication_by_prefix(pub_prefix, 1, REP.RETURN_ZERO);
  int lde_def_rev = current_lde_def_rev(pub);
  mapping(string:mixed) fields = ([
    "publication_prefix" : pub_prefix,
    "property_id"        : user_prop,
    "revision"           : lde_def_rev,
    "required_at"        : def["required-level"] || "",
    "type"               : def["type"],
  ]);

  // FIXME: Ought to use object_insert_or_update() or object_replace(),
  //        but they don't exist.
  REP.ObjectTable tbl = user_property_defs_table();
  array(REP.PropDef) existing = tbl->
    object_select("publication_prefix = '" + tbl->quote(pub_prefix || "") + "' "
                  "AND property_id = '" + tbl->quote(user_prop) + "'");
  REP.PropDef propdef =
    (sizeof(existing) && existing[0]) ||
    tbl->object_create(fields, on_error);

  //  Now set (or update) fields including default value using type coercion
  if (propdef) {
    propdef->set_fields(fields);
    mixed cast_default_value = propdef->cast_to_prop_type(def["default"]);
    propdef->set_fields( ([ "default" : cast_default_value ]) );
  }

  REP.get_print_module()->invalidate_user_propdefs_cache();

  return propdef;
}

// Publications

array(REP.Publication) list_publications()
//! List all publications and return an array of their
//! @[REP.Publication] objects. The array is sorted on publication
//! title (but not with a locale-specific sort algorithm).
{
  REP.PrintDBModule print_db = REP.get_print_module();
  array(REP.Publication) res = print_db->all_publ_cache;
  if (!res)
    res = print_db->all_publ_cache =
      publications_table()->object_select ("TRUE", 0, "ORDER BY title");
  return res;
}

REP.Publication publication_by_id(int(1..) publication_id,
                                  void|REP.OnError on_rec_not_found) {

  REP.ObjectTable tbl = publications_table();
  return tbl->object_get(publication_id, on_rec_not_found);
}

REP.Publication publication_by_uuid(string publication_uuid,
                                    void|REP.OnError on_rec_not_found)
{
  REP.ObjectTable tbl = publications_table();
  REP.Publication publ = publication_by_id (rec_id_by_uuid (tbl,
                                                            publication_uuid),
                                            REP.RETURN_ZERO);

  if (publ)
    return publ;

  return REP.raise_err(on_rec_not_found,
                       "Publication with uuid %s not found.\n",
                       publication_uuid);
}

REP.Publication publication_by_prefix(string prefix,
                                      void|int include_deleted,
                                      void|REP.OnError on_rec_not_found) {
  // FIXME: Consider a lookup mapping if this function is used
  // frequently.

  //  Prefixes must not differ only in caseness since the underlying file
  //  system representation cannot always make a distinction of case alone.
  prefix = lower_case(prefix);

  foreach (list_publications(), REP.Publication publ)
    if (lower_case(publ->get ("prefix")) == prefix) {
      if (include_deleted || !publ->get ("is_deleted"))
        return publ;
      break;
    }

  return REP.raise_err (on_rec_not_found, "Unknown publication prefix %O.\n",
                        prefix);
}


constant invalid_pub_prefixes = (< "db-objs", "spool", "spool-errors" >);
//! Publication prefixes (in lower-case form) that are invalid due to name
//! clash in the print/db/ storage.


//! Factory method for creating new publications, ie not just fetch
//! object representations of existing ones. If an old, deleted,
//! publication with the same prefix is found, that one is reused by
//! undeleting it an renaming it.
//! @param title
//!        The title of the new publication.
//! @param prefix
//!        The prefix of the new publication.
//! @param init_values
//!        Mapping with initial values for other properties than title and prefix.
//! @returns
//!        A new publication object.
REP.Publication create_publication(string title,
                                   string prefix,
                                   void|REP.OnError on_rec_exists,
                                   void|mapping(string:mixed) init_values,
                                   void|REP.OnError on_invalid_rec)
{

  REP.Publication old = publication_by_prefix(prefix, 1, REP.RETURN_ZERO);

  if (old) {
    if (!old->get("is_deleted")) {
      return REP.raise_err(on_rec_exists,
                           "A publication with prefix %s already exists!\n",
                           prefix);
    }

    mapping(string:mixed) init_rec = init_values || ([]);
    init_rec->title = title;
    init_rec->is_deleted = 0;
    old->low_set_raw (init_rec);
    old->set_default_user_properties();
    return old;
  }

  else {
    for (int i = 0; i < sizeof (prefix); i++) {
      int c = prefix[i];
      if (!((c >= '0' && c <= '9') ||
            (c >= 'a' && c <= 'z') ||
            (c >= 'A' && c <= 'Z')))
        return REP.raise_err (on_invalid_rec, "Invalid characters in prefix.\n");
    }
    if (invalid_pub_prefixes[lower_case(prefix)]) {
      return REP.raise_err(on_invalid_rec, "Reserved prefix.\n");
    }

    mapping(string:mixed) init_rec = init_values || ([]);

    init_rec->title = title;
    init_rec->prefix = prefix;

    REP.Publication pub = publications_table()->object_create(init_rec);
    REP.get_print_module()->all_publ_cache = 0;
    return pub;
  }
}



//  Formatter for page numbers according to jump format setting in a section
string format_jump_page_number(REP.PageSlot ps)
{
  //  The format string will be something like "A1", "A-1" or "1".
  //  C.f. REP.Edition.SectionDef.default_jump_formats.
  string fmt = ps->section_def()->jump_format();
  string marker = ps->section_marker();
  string page_no = (string) ps->get("calc_page_no");
  return replace(fmt, ([ "A": marker, "1": page_no ]) );
}


//  User-configurable page variables. These can be global (for all editions
//  or a particular name), associated to a publication prefix or specific to
//  an edition name. A more specific occurrence will override a global one.
//
//  Variable values are text-based and the names will be prefixed with
//  "REP:" to ensure uniqueness in InDesign.
mapping(string:string) get_default_text_variables_for_pgv(REP.PGVersion pgv)
{
  array(REP.PageSlot) slots =
    REP.DB.sort_page_slots_by_order(pgv->pages()->page_slot() - ({ 0 }));
  if (!sizeof(slots))
    return ([ ]);

  REP.Edition ed = pgv->edition();
  REP.PageSlot ps = slots[0];
  REP.Edition.SectionDef section_def = ps->section_def();

  return ([
    //  Counterparts to %{section} and %{section_name}
    "REP:Section": ps->section_marker(),
    "REP:Section-Name": (section_def && section_def->label()) || "",

    //  Counterparts to %{zone_code} and %{cone_name}
    "REP:Zone-Code": ed->ze_zone_code() || "",
    "REP:Zone-Name": ed->ze_zone_label() || "",

    //  Counterparts to %{edition_code}
    "REP:Edition-Code": ed->ze_edition_code() || ""
  ]);
}


mapping(string:string)|int(0..0) get_text_variables_for_pgv(REP.PGVersion pgv)
{
  ASSERT_IF_DEBUG(pgv);

  REP.Edition ed = pgv->edition();
  REP.Publication publ = ed->publication();
  string ed_title = ed->get("title");
  mapping(string:string) res = ([ ]);

  if (REP.PrintDBModule print_db = REP.get_print_module()) {
    mapping(string:mapping(string:mapping(string:string))) all_vars =
      print_db->get_all_text_variables();
    //  Add system defaults for section, zone and editioning. These may be
    //  overridden by user-defined entries below.
    res += get_default_text_variables_for_pgv(pgv);

    //  Global variables
    if (mapping(string:mapping) global_vars = all_vars[""]) {
      res += (global_vars[""] || ([ ]) ) + (global_vars[ed_title] || ([ ]) );
    }

    //  Publication- and edition-specific variables
    if (mapping(string:mapping) publ_vars = all_vars[publ->get("prefix")]) {
      res += (publ_vars[""] || ([ ]) ) + (publ_vars[ed_title] || ([ ]) );
    }
  }

  foreach (res; string index; string value) {
    res[index] = REP.Storage.format_external_filename_wide (value, pgv);
  }

  return sizeof(res) && res;
}

mapping(string:mapping(string:string))|int(0..0)
  get_story_jump_variables_for_pgv(REP.PGVersion pgv)
{
  ASSERT_IF_DEBUG(pgv);

  REP.Edition ed = pgv->edition();
  if (ed->is_template())
    return 0;

  mapping(string:REP.TextFlowLink) cur_tfls = pgv->cur_text_flow_links();
  mapping res = ([ ]);
  foreach (cur_tfls; string tfl_uuid; REP.TextFlowLink tfl) {
    array(REP.Story) stories = tfl->stories_in_link();
    foreach (stories, REP.Story s) {
      //  Jump title
      string jump_title = s->get_jump_title();

      //  Jump source and destination page slots. The identified page groups
      //  may contain multiple pages, but the story may not be touching all
      //  of them. We locate the first or last page in the PGV where the
      //  story has touching page items.
      //
      //  We exclude page subspec in these labels since that's mostly a
      //  production detail and not reader-visible.
      string prev_page, next_page;
      if (REP.TextFlowLink prev_link = tfl->preceding_link()) {
        if (REP.PageSlot last_ps = prev_link->get_last_page_slot(s)) {
          prev_page = format_jump_page_number(last_ps);
        }
      }
      if (REP.TextFlowLink next_link = tfl->following_link()) {
        if (REP.PageSlot first_ps = next_link->get_first_page_slot(s)) {
          next_page = format_jump_page_number(first_ps);
        }
      }

      res[(string) s->story_doc_id(pgv)] = ([ "jump_title": jump_title,
                                              "prev_page": prev_page,
                                              "next_page": next_page ]);
    }
  }

  return sizeof(res) && res;
}

void|mapping(string:int(1..1)) pgv_jump_preflight(REP.PGVersion pgv)
{
  //  Same jump checks as in REP.Story::outdated_placements_in_edition().
  //
  //  We skip jump head/reflow warnings on shadow pages since there will
  //  be false positives due to text flow link hashes that originate from
  //  the shadow parent.
  mapping res = ([ ]);
  if (!pgv->is_shadow()) {
    mapping(string:REP.TextFlowLink) cur_tfls = pgv->cur_text_flow_links();
    foreach (cur_tfls; string tfl_uuid; REP.TextFlowLink tfl) {
      //  Missing head snippet trumps reflow need in same chain
      if (tfl->chain_missing_head_text()) {
        res["jump-head"] = 1;
      } else {
        [int update_type, REP.PGVersion src_pgv,
         REP.Storage.FileType src_snippet] = tfl->reflow_source();
        if (update_type == 1)
          res["jump-reflow"] = 1;
      }
    }
  }

  //  Cross-ref error could also refer to same story that lacks head snippet,
  //  but this is not easily checked. We'll report both to cover the case
  //  where the errors are for independent stories.
  if (pgv_needs_jump_variable_update(pgv))
    res["jump-xref"] = 1;

  return sizeof(res) && res;
}

bool pgv_needs_jump_variable_update(REP.PGVersion pgv)
{
  //  Compare what the page would require vs what we have saved on last
  //  deconstruct. If the page is too old to have layout info or even
  //  jump variable extraction we don't raise a flag; such pages pre-date
  //  the new story jump implementation and isn't relevant to handle.
  mapping li = pgv->get("doc_layout_info");
  if (!mappingp(li) || !mappingp(li->jump_vars))
    return false;

  //  NOTE: If this becomes a bottleneck with many stories on pages,
  //        consider a step-by-step check that fetches doc ID strings
  //        first for a rough check, and only if matching dig deeper
  //        into prev/next references. The slot analysis code in the
  //        jump variable fetching processes layout page items for all
  //        occurrences right now.
  if (mapping(string:mapping(string:string)) story_jump_vars =
      get_story_jump_variables_for_pgv(pgv)) {
    //  The "jump_vars" record from layout info should be structured as
    //  follows:
    //
    //    ([ "-1015": ([ "count": 2,
    //                   "jump_title": "Foobar",
    //                   "next_page": "C16",
    //                   "prev_page": 0 ]),
    //       "-1284": ([ ... ]),
    //       [...]
    //    ])
    //
    //  Note the "count" key which counts the number of times any of the
    //  three variables (i.e. aggregated per story doc id) is used in the
    //  layout. We only care for non-zero counts since we don't need to
    //  warn for unused variables (which are expected for non-jump articles).
    mapping(string:mapping(string:string)) cur_page_jump_vars = li->jump_vars;

    foreach (story_jump_vars; string doc_id_str; mapping story_jv) {
      mapping(string:string) cur_page_jv = cur_page_jump_vars[doc_id_str];
      if (!cur_page_jv ||
          (cur_page_jv->count &&
           !equal(cur_page_jv - ({ "count" }), story_jv))) {
        //  Either new document ID or its variable values are different
        return true;
      }
    }
  }

  //  Not using any jump variables (e.g. a template page) means we're ok
  return false;
}

mapping(string:mapping(string:string))
  get_new_story_placement_story_jump_variables(void|REP.PGVersion pgv,
                                               REP.Story s)
{
  string story_doc_id_str = (string) s->story_doc_id(pgv);
  if (pgv) {
    //  Use existing jump variables for this doc ID if available
    if (mapping(string:mapping(string:string|int)) pgv_jump_vars =
        REP.DB.get_story_jump_variables_for_pgv(pgv)) {
      if (mapping cur_story_jump_vars = pgv_jump_vars[story_doc_id_str])
        return ([ story_doc_id_str: cur_story_jump_vars ]);
    }
  }

  return ([ story_doc_id_str: ([ "jump_title": s->get_jump_title(),
                                 "prev_page": 0,
                                 "next_page": 0 ]) ]);
}

mapping(string:mapping(string:string))|int(0..0)
  get_new_jump_placement_story_jump_variables(REP.PGVersion pgv,
                                              REP.Story s, string tc_uuid)
{
  //  We cannot look for the text chain associated to the PGV since it is
  //  about to be placed. Instead we find it through the UUID.
  ASSERT_IF_DEBUG(pgv);

  REP.Edition ed = pgv->edition();
  if (ed->is_template())
    return 0;

  mapping(string:array(REP.TextFlowLink)) ed_text_chains =
    ed->get_text_chains();
  if (array(REP.TextFlowLink) tfls = ed_text_chains[tc_uuid]) {
    //  Jump title
    string jump_title = s->get_jump_title();

    //  For a new placement the previous page reference will be the last
    //  in the chain, and the next page is empty.
    REP.TextFlowLink last_tfl = tfls[-1];
    string prev_page;
    if (REP.PageSlot last_ps = last_tfl->get_last_page_slot(s)) {
      prev_page = format_jump_page_number(last_ps);
    }

    return ([ (string) s->story_doc_id(pgv): ([ "jump_title": jump_title,
                                                "prev_page": prev_page,
                                                "next_page": 0 ]) ]);
  }
  return 0;
}



// Miscellaneous

int get_story_item_status_id (string story_item_status,
                              void|REP.OnError on_not_found)
{
  if (int res =
      REP.get_print_module()->get_story_item_status_id (story_item_status, 1))
    return res;
  return REP.raise_err (on_not_found, "Unknown story item status %O.\n",
                        story_item_status);
}

string get_story_item_status (int story_item_status_id,
                              void|REP.OnError on_not_found)
{
  return REP.get_print_module()->get_story_item_status (story_item_status_id,
                                                        on_not_found);
}

int get_file_type_id (REP.Storage.FileType file_type,
                      void|REP.OnError on_not_found)
//! Returns the table id for the given file type.
//!
//! @note
//! This function is hopefully a temporary measure.
{
  if (REP.Storage.is_pdf_preview (file_type))
    file_type = "pdf-preview";
  if (int res = REP.get_print_module()->get_file_type_id (file_type, 1))
    return res;
  return REP.raise_err (on_not_found, "Unknown file type %O.\n", file_type);
}

int get_page_type_id (string page_type, void|REP.OnError on_not_found)
{
  if (int res = REP.get_print_module()->get_page_type_id (page_type, 1))
    return res;
  return REP.raise_err (on_not_found, "Unknown page type %O.\n", page_type);
}

string get_page_type (int page_type_id, void|REP.OnError on_not_found)
{
  return REP.get_print_module()->get_page_type (page_type_id, on_not_found);
}

int get_page_status_id (string page_status, void|REP.OnError on_not_found)
{
  if (int res = REP.get_print_module()->get_page_status_id (page_status, 1))
    return res;
  return REP.raise_err (on_not_found, "Unknown page status %O.\n", page_status);
}

string get_page_status (int page_status_id, void|REP.OnError on_not_found)
{
  return REP.get_print_module()->get_page_status (page_status_id, on_not_found);
}

int pdf_settings_generation (REP.Publication publ)
{
  //  Note that this generation counter also affects InDesign package validity
  return publ->get ("pdf_settings_gen");
}

int bump_pdf_settings_generation (REP.Publication publ)
{
  // The counter always increases so the race should be harmless.
  publ->set_fields (([ "pdf_settings_gen": publ->get ("pdf_settings_gen")+1 ]));
  return publ->get ("pdf_settings_gen");
}



// LDE -- multilingual labels

mapping(string:string) get_label(int label_id)
//!  Queries the labels table for a given ID. The returned mapping will
//!  contain entries for all languages for which the label is defined.
//!
//!  @param label_id
//!    The ID of the existing label.
//!  @returns
//!    A mapping from language to label string, or an empty mapping for
//!    unknown IDs. Don't be destructive on it.
{
  if (!label_id) return ([]);
  REP.CachedTable tbl = labels_table();
  mapping(string:string) names = ([]);
  mapping(string:mixed) rec =
    tbl->cached_get (label_id, REP.RETURN_ZERO);
  if (rec) names = rec->langs;
  return names;
}

int set_label(int label_id, mapping(string:string) label)
//!  Updates an existing label with new values or adds a new label to
//!  the database. If fewer languages are present in this label
//!  compared to the old version the additional strings are deleted.
//!
//!  @param label_id
//!    The ID of the existing label, or 0 to create a new label.
//!  @param label
//!    A mapping from language to label string.
//!  @returns
//!    The label ID if an entry was added or modified and 0 if not found.
{
  ASSERT_IF_DEBUG (mappingp (label));
  REP.CachedTable tbl = labels_table();
  mapping rec = ([ "langs" : label ]);

  if (label_id) {
    rec->label_id = label_id;
    if (!tbl->cached_get (label_id, REP.RETURN_ZERO))
      return 0;

    tbl->cached_update (rec);
  } else {
    return tbl->cached_insert (rec);
  }
}

void remove_label(int label_id)
//!  Removes a label referenced by a given ID from the database. Does
//!  nothing if the ID cannot be found.
//!
//!  @param label_id
//!    The ID of the existing label.
{
  REP.CachedTable tbl = labels_table();
  tbl->cached_remove (label_id);
}

string select_label(mapping(string:string) label,
                    void|array(string) extra_langs)
//! Selects a label from a mapping from language to label.
//!
//! @param label
//!   The mapping from language to label value.
//!
//! @param extra_langs
//!   Normally the languages in the current @[REP.ClientSession] are
//!   used to select a suitable language. This may specify additional
//!   languages when there is no active client session, or none of its
//!   languages fit. May be zero.
//!
//! @note
//!   If both @[cs] and @[extra_langs] are provided, the union of
//!   languages from both of them will be used with @[cs] taking
//!   precedence.
{
  array(string) languages = ({});
  if (REP.ClientSession cs = REP.get_session()->cs) languages += cs->languages;
  if (extra_langs) languages += extra_langs;
  languages = Array.uniq (languages);

  //  Select most appropriate label based on the user's preferred language.
  //
  //  As a friendly help to avoid spurious label missing errors we always
  //  include "en" as fallback. We may still fail for non-user sessions
  //  with weird language configs but labels shouldn't be cached in objects
  //  anyway so it should be invisible.
  if (!has_value(languages, "en"))
    languages += ({ "en" });
  foreach(languages, string lang)
    if (string val = label[lang])
      return val;
  return "(Label missing for language " +
    (sizeof (languages) ? "'" + languages[0] + "'" : "(no language given)") +
    ")";
}


//  LDE -- story template and component definitions

private void set_lde_parameters(mapping params, string type, int parent_id)
{
  mapping base = ([ "type"   : type,
                    "def_id" : parent_id ]);
  array(mapping) recs = ({ });
  foreach (params; string name; string val) {
    mapping rec = ([ "name"  : name,
                     "value" : val ]) + base;
    recs += ({ rec });
  }
  parameters_table()->insert(@recs);
}


REP.StoryTemplateDef
create_lde_story_template_def(int(0..0)|string pub_prefix,
                              mapping(string:mixed) fields,
                              int config_revision,
                              void|REP.OnError on_value_error)
//!  Creates a new story template definition record in the SQL
//!  database, together with any referenced labels, parameters and
//!  constraints. Related component definitions must be added using
//!  @[create_lde_component_def] before calling this method.
//!
//!  @param pub_prefix
//!    The publication for which this LDE configuration applies to, or zero
//!    for global definitions.
//!
//!  @param fields
//!    Story template definition structure returned by the LDE config parser.
{
  //  Story template record
  mapping geom = fields->geometry;
  mapping(string:mixed) tmpl_rec =
    ([ "name"               : fields->id,
       "publication_prefix" : pub_prefix || "",
       "revision"           : config_revision,
       "label_id"           : set_label(0, fields->label) ]);
  REP.StoryTemplateDef tmpl_def =
    story_tmpl_defs_table()->object_create(tmpl_rec, on_value_error);
  int tmpl_def_id = tmpl_def->id();

  //  Parameters
  if (sizeof(fields->params))
    set_lde_parameters(fields->params, "story", tmpl_def_id);

  //  Constraints
  if (array(mapping) constrs = geom && geom->constraints) {
    if (sizeof (constrs)) {
      array(mapping(string:mixed)) constr_recs = ({ });
      foreach (constrs, mapping constr) {
        mapping(string:mixed) constr_rec =
          ([ "story_tmpl_def_id" : tmpl_def_id,
             "apply_to"          : constr->apply_to,
             "name"              : constr->name,
             "value"             : constr->value ]);
        constr_recs += ({ constr_rec });
      }
      constraints_table()->insert(@constr_recs);
    }
  }

  //  Create component area definitions for story template.
  array(mapping(string:int)) ref_recs = ({ });
  foreach (fields->refs; int i; mapping(string:mixed) ref) {
    mapping(string:mixed) comp_area_rec =
      ([ "story_tmpl_def_id" : tmpl_def_id,
         "component_def_id"  : ref->ref_id,
         "name"              : ref->name,
         "order"             : i+1,
         "size"              : ref->size,
         "label_id"          : ref->opt_label && set_label(0, ref->opt_label)
      ]);

    component_area_defs_table()->object_create (comp_area_rec, on_value_error);
  }

  REP.get_print_module()->clear_cached_tmpl_ids();
  return tmpl_def;
}

REP.StoryTemplateDef
create_derived_story_template_def (
  REP.StoryTemplateDef base_def,
  REP.PGVersion src_pgv,
  string src_ext_page_id,
  int src_page_item_id,
  string display_name,
  mapping(string:REP.ComponentDef|mapping(string:mixed)) component_data,
  void|array(string) geometry_snippets,
  void|array(mapping) geometry_bounds,
  void|array(string) publ_prefix_visibility,
  void|array(mapping) autolayout,
  void|int(0..1) is_jump_template,
  void|REP.OnError on_value_error)
{
  string pub_prefix = base_def->get ("publication_prefix");

  // Note: derived templates use the storage counter of the PGVersion
  // they got created from as their revision. This enables correct
  // updating and deletion of derived templates from template pages.

  mapping(string:mixed) tmpl_rec =
    ([ "name"               : REP.IDS.mangle_template_name (display_name),
       "label_id"           : set_label (0, ([ "en": display_name ])),
       "publication_prefix" : pub_prefix,
       "revision"           : src_pgv->get ("storage_counter"),
       "parent_id"          : base_def->id(),
       "source_pgv_id"      : src_pgv->id(),
       "source_ext_page_id" : src_ext_page_id,
       "source_page_item_id": src_page_item_id,
       "autolayout"         : autolayout,
       "is_jump_template"   : is_jump_template
    ]);

  //  Add publication visibility
  if (publ_prefix_visibility) {
    if (!base_def->publication()) {
      tmpl_rec->publication_visibility = publ_prefix_visibility;
    } else {
      werror("Cannot set publication visibility of a story template in "
             "non-global space (base def: %O).\n", base_def);
    }
  }

  if (geometry_snippets) {
    //  External storage of array(string)
    tmpl_rec->geometry_snippet = geometry_snippets;

    //  This is the best preview we have so far, but the first time a story
    //  is placed server-side using this snippet we will also pre-parse the
    //  markup and get a washed preview (sans blue and yellow designators)
    //  as a bonus.
    array(string) raw_jpegs =
      map(geometry_snippets, REP.IDS.get_snippet_jpeg_preview);
    if (sizeof(raw_jpegs - ({ 0 }) ))
      tmpl_rec->geometry_snippet_preview = raw_jpegs;

    //  Set initial bounds if known
    if (geometry_bounds)
      tmpl_rec->geometry_snippet_bounds = geometry_bounds;
  }

#if 0
  // Bump revision in current def before enabling this optimization.
  if (REP.StoryTemplateDef current_def =
      story_template_def_for_publication (base_def->publication(),
                                          tmpl_rec->name)) {
    if (current_def->parent_id == tmpl_rec->parent_id &&
        current_def->geometry_snippet == tmpl_rec->geometry_snippet)
      return;
  }
#endif

  REP.StoryTemplateDef new_def =
    story_tmpl_defs_table()->object_create(tmpl_rec, on_value_error);
  int new_def_id = new_def->id();

  Sql.Sql db = REP.get_db();

  // Copy parameters
  if (mapping(string:mixed) story_tmpl_params = base_def->get_parameters())
    if (sizeof (story_tmpl_params))
      set_lde_parameters (story_tmpl_params, "story", new_def_id);

  // Copy constraints
  array(mapping(string:mixed)) new_constr_recs = ({ });
  foreach (db->query ("SELECT story_tmpl_def_id, apply_to, name, value "
                      "  FROM constraints "
                      " WHERE story_tmpl_def_id = %d",
                      base_def->id()),
           mapping(string:mixed) row) {
    new_constr_recs += ({ ([ "story_tmpl_def_id" : new_def_id,
                             "apply_to"          : row->apply_to,
                             "name"              : row->name,
                             "value"             : row->value ]) });
  }
  if (sizeof(new_constr_recs)) {
    constraints_table()->insert(@new_constr_recs);
    new_def->flush_constraints_cache();
  }

  // Create new component area defs based on the ones from base_def,
  // but with the correct sizes.
  array(string) components = indices (component_data);
  sort (components);
  array(mapping(string:mixed)) res_comps = ({});
  multiset(REP.ComponentAreaDef) used_ca_defs = (<>);

  int(0..1) field_mappings_equal (mapping(string:mixed) m1,
                                  mapping(string:mixed) m2)
  {
    if (!m1 && !m2) return 1;
    if (!m1 && m2) return 0;
    if (m1 && !m2) return 0;

    foreach (m1; string name; mixed data)
      if (m2[name] && !equal(data, m2[name]))
        return 0;

    return 1;
  };

  mapping(string:int) comp_name_count = ([ ]);
  foreach (components, string comp_spec) {
    mapping(string:mixed)|REP.ComponentDef data = component_data[comp_spec];
    sscanf (comp_spec, "%s#%d", string comp_name, int count);
    REP.ComponentAreaDef ca_def =
      base_def->component_area_def_by_comp_name (comp_name);

    if (!ca_def) {
      werror ("No component area def / component def named %O found.\n",
              comp_name);
      continue;
    }

    used_ca_defs[ca_def] = 1;

    mapping(string:mixed) prev_comp_data =
      sizeof (res_comps) && res_comps[-1];
    if (prev_comp_data &&
        field_mappings_equal (data->fields, prev_comp_data->fields) &&
        comp_name == prev_comp_data->comp_name) {
      // Identical field specs, we can reuse the same entry to avoid
      // unnecessary component area defs below.
      prev_comp_data->size++;
      continue;
    }

    data->comp_name = comp_name;
    data->size = 1;
    data->ca_def = ca_def;

    res_comps += ({ data });
    comp_name_count[comp_name]++;
  }

  sort (res_comps->ca_def->order(), res_comps);

  int new_order = 1;
  mapping(string:int) comp_name_offset = ([ ]);
  mapping(REP.ComponentDef:int) override_area_label = ([ ]);

  foreach (res_comps, mapping(string:mixed) data) {
    REP.ComponentAreaDef orig_ca_def = data->ca_def;
    REP.ComponentDef new_comp_def;

    if (new_comp_def = data->comp_def) {
      // Component def already created by
      // handle_new_component_template (print-indesign-server).
    } else {
      REP.ComponentDef orig_comp_def = orig_ca_def->component_def();
      mapping(string:array(mapping(string:mixed))) styles_by_field = ([]);

      foreach (orig_comp_def->get_field_defs(), REP.FieldDef field_def) {
        array(mapping(string:mixed)) styles = ({});
        string field_name = field_def->get_name();
        if (mapping(string:mixed) new_field_info = data->fields[field_name]) {
          foreach (({ "para", "char" }), string type) {
            foreach (new_field_info[type + "Styles"] || ({ }),
                     mapping style_info) {
              string disp_name = style_info.label;
              mapping style_attrs = style_info.attrs || ([ ]);

              mapping(string:mixed) style_props;
              if (style_props = style_attrs.styleProps) {
                mapping(string:int) valid_props =
                  REP.FieldStyle.valid_props[type];
                ASSERT_IF_DEBUG (!sizeof (style_props - valid_props));
                style_props = REP.DB.decode_objects (style_props);
              }

              styles += ({
                ([ "type"               : type,
                   "name"               : Sitebuilder.mangle_to_09_az(disp_name),
                   "label_id"           : set_label(0, ([ "en": disp_name ]) ),
                   "style_extref"       : style_info.extref,
                   "style_extref_group" : style_info.extref_group,
                   "css"                : style_attrs.css || "",
                   "props"              : style_props,
                ]) });
            }
          }
        }

        if (sizeof (styles)) styles_by_field[field_name] = styles;
      }

      int rev = src_pgv->get ("storage_counter");
      mapping(string:mixed) comp_def_fields = ([
        // Assign unique names to autocreated components to avoid name
        // clashes with base component definitions.
        "name"               : orig_comp_def->get_name() + sprintf ("-%d", rev),
        "revision"           : rev,
        "source_pgv_id"      : src_pgv->id(),
        "parent_id"          : orig_comp_def->id(),
        "is_auto_created"    : 1,
      ]);

      new_comp_def = copy_component_def (orig_comp_def, comp_def_fields,
                                         styles_by_field);

      //  If this component name appears multiple times we need to give each
      //  area a unique label. For instance, having four image captions in a
      //  template where the first uses different styles will force us to
      //  have extra "Image" areas instead of unifying them. We then wish to
      //  name them e.g. "Image (#1)" and "Image (#2-#4)" to indicate that
      //  the first one expects one item and the other three items.
      if (comp_name_count[data->comp_name] > 1) {
        //  We keep a running counter for each name to track number of
        //  previous item placeholders inserted.
        int idx_start = comp_name_offset[data->comp_name] + 1;
        int idx_end = idx_start + data->size - 1;
        comp_name_offset[data->comp_name] = idx_end;
        string idx_range_str =
          (idx_start == idx_end) ?
          (" (#" + idx_start + ")") :
          (" (#" + idx_start + "-#" + idx_end + ")");
        mapping(string:string) new_ca_label =
          orig_ca_def->display_name() + ([ ]);
        foreach (new_ca_label; string lang; string label)
          new_ca_label[lang] += idx_range_str;
        override_area_label[new_comp_def] = set_label(0, new_ca_label);
      }
    }

    mapping(string:mixed) comp_area_rec = orig_ca_def->get_rec();
    m_delete (comp_area_rec, "component_area_def_id");
    comp_area_rec->component_def_id = new_comp_def->id();
    comp_area_rec->size = mappingp (data) ? data->size : 1;
    comp_area_rec->order = new_order++;
    if (int override_label_id = override_area_label[new_comp_def])
      comp_area_rec->label_id = override_label_id;

    comp_area_rec->story_tmpl_def_id = new_def_id;
    component_area_defs_table()->object_create (comp_area_rec, on_value_error);
  }

  // Copy the component area defs that haven't been used in the
  // layout. C.f. InfoKOM #771173.
  foreach ((multiset)base_def->component_area_defs() - used_ca_defs;
           REP.ComponentAreaDef ca_def;) {
    mapping(string:mixed) comp_area_rec = ca_def->get_rec();
    m_delete (comp_area_rec, "component_area_def_id");
    comp_area_rec->story_tmpl_def_id = new_def_id;
    component_area_defs_table()->object_create (comp_area_rec, on_value_error);
  }

  base_def->clear_derived_defs_cache();
  REP.get_print_module()->clear_cached_tmpl_ids();
  return new_def;
}


void reinstantiate_derived_defs (array(REP.TemplateDef) derived_defs)
{
  mapping(int:int) extract_snippet_pg_ids = ([]);

  foreach (derived_defs, REP.TemplateDef def) {
    if (int pgv_id = def->get ("source_pgv_id")) {
      REP.PGVersion pgv = pgv_by_id (pgv_id, REP.RETURN_ZERO);
      if (!pgv) {
        // The referenced source_pgv_id may have been purged, but a
        // newer version with the same storage counter may exist
        // instead.
        int rev = def->get_revision();
        array(REP.PGVersion) pgvs_by_cnt = REP.DB.pgvs_by_storage_counter (rev);
        pgv = sizeof (pgvs_by_cnt) && pgvs_by_cnt[0];
      }

      //  Skip trashed template documents
      if (pgv && pgv->is_placed())
        extract_snippet_pg_ids[pgv->get ("page_group_id")] = 1;
    }
  }

  foreach (extract_snippet_pg_ids; int pg_id;) {
    // Create a new pgv and resave the document to get a new
    // deconstruct with potential error messages originating from that
    // the base def changed.
    REP.PGVersion cur_pgv = current_pgv_by_pg_id (pg_id);
    REP.PGVersion new_pgv;
    do {
      cur_pgv = cur_pgv->current_pgv();
      new_pgv = add_pgv (cur_pgv,
                         -1, // Overwriteable
                         ([ "comment" : "Base story definition changed" ]));
      string src_filepath =
        cur_pgv->get_existing_layout_filepath (REP.RETURN_ZERO) ||
        cur_pgv->get_unproc_filepath();

      new_pgv->store_file_by_copy (REP.get_print_module()->get_file_db_path() +
                                   src_filepath,
                                   REP.Storage.UNPROC_LAYOUT_FILE,
                                   cur_pgv->layout_mime_type(),
                                   UNDEFINED,
                                   UNDEFINED,
                                   cur_pgv);
    } while (!REP.DB.make_pgv_current (new_pgv, cur_pgv, REP.RETURN_ZERO));
  }

  array(REP.TemplateDef) parent_defs =
    Array.uniq (derived_defs->get_parent()) - ({ 0 });

  parent_defs->clear_derived_defs_cache();
  REP.get_print_module()->clear_cached_tmpl_ids();
}


protected array(mapping(string:mixed))
get_lde_metadata_field_list (REP.ComponentDef comp_def)
// Create metadata fields that will be commited to the DB together
// with component defintions from XML config.
{
  array(mapping(string:mixed)) meta_field_defs = ({});

  string wf_type;
  if (has_prefix (comp_def->get_class(), "rep.text"))
    wf_type = "text";
  else if (has_prefix (comp_def->get_class(), "rep.image"))
    wf_type = "image";

  if (wf_type) {
    mapping(string:mixed) field_def = ([]);
    field_def->name = "__status";
    field_def->label = ([ "en" : "Status",
                          "sv" : "Status" ]);
    field_def->type = "selector";

    array(REP.StatusLevel) levels =
      REP.get_print_module()->get_status_levels_by_type (wf_type);

    if (levels) {
      field_def->params =
        ([ "options" :
           map (levels,
                lambda (REP.StatusLevel sl)
                {
                  return ([ "name"     : sl->id,
                            "label_id" :
                            set_label (0, ([ "en" : sl->display_name ])),
                            "color"    : sl->color ]);
                })
        ]);

      field_def->initial_val = levels[0]->id;

      field_def->para_styles = field_def->char_styles =
        field_def->table_styles = ([]);

      meta_field_defs += ({ field_def });
    }
  }

  return meta_field_defs;
}


REP.ComponentDef create_lde_component_def(int(0..0)|string pub_prefix,
                                          mapping(string:mixed) fields,
                                          int config_rev,
                                          void|REP.OnError on_value_error)
//!  Creates a new component definition record in the SQL database,
//!  together with any referenced labels, fields, parameters and
//!  styles.
//!
//!  @param fields
//!    Component definition structure returned by the LDE config parser.
{
  //  Component definition record
  mapping(string:mixed) comp_rec =
    ([ "name"               : fields->name,
       "publication_prefix" : pub_prefix || "",
       "label_id"           : set_label(0, fields->label),
       "revision"           : config_rev,
       "class"              : fields->class ]);
  REP.ComponentDef comp_def =
    component_defs_table()->object_create(comp_rec, on_value_error);
  int comp_def_id = comp_def->id();

  //  Field definitions. Note that these are ordered in the caller's data
  //  structure by a "pos" member and in the database through the
  //  auto-increment ID.
  array(mapping) fields_list =
    get_lde_metadata_field_list (comp_def) +
    values(fields->fields);
  sort(fields_list->pos, fields_list);

  mapping(int:mapping(string:mixed)) data_provider_fields = ([]);

  // Note: if changes are made to this initialization code (field
  // defs, styles, parameters), make sure copy_field_def reflects the
  // changes.
  foreach (fields_list, mapping field_entry) {
    // Assertions for required items in the entry.
    ASSERT_IF_DEBUG (field_entry /*%O*/->name, field_entry);
    ASSERT_IF_DEBUG (field_entry /*%O*/->label, field_entry);
    ASSERT_IF_DEBUG (field_entry /*%O*/->type, field_entry);

    //  Insert field
    mapping(string:mixed) field_rec =
      ([ "component_def_id" : comp_def_id,
         "name"             : field_entry->name,
         "label_id"         : set_label(0, field_entry->label),
         "type"             : field_entry->type ]);
    if (!zero_type (field_entry->initial_val))
      field_rec["initial_val"] = field_entry->initial_val;
    if (!zero_type (field_entry->initial_from_body_style))
      field_rec["initial_from_body_style"] =
        field_entry->initial_from_body_style;
    if (!zero_type (field_entry->initial_from_ac))
      field_rec["initial_from_ac"] = field_entry->initial_from_ac;

    int field_def_id = field_defs_table()->object_create(field_rec)->id();

    if (!zero_type (field_entry->data_provider_field)) // Mapping.
      data_provider_fields[field_def_id] = field_entry->data_provider_field;

    //  Parameters
    if (field_entry->params && sizeof(field_entry->params)) {
      mapping params = field_entry->params;
      foreach ((params && params->options) || ({}), mapping options) {
        mapping label = m_delete (options, "label");
        if (label) options->label_id = set_label (0, label);
      }

      set_lde_parameters(field_entry->params, "field", field_def_id);
    }

    //  Styles. Again ordered by a "pos" member in input and by
    //  auto-increment ID in the database.
    array(mapping) style_list =
      values(field_entry->para_styles || ([])) +
      values(field_entry->char_styles || ([])) +
      values (field_entry->table_styles || ([]));
    sort(style_list->pos, style_list);
    foreach (style_list, mapping style_entry) {
      string type;
      if (style_entry->type == "paragraph")
        type = "para";
      else if (style_entry->type == "character")
        type = "char";
      else if (style_entry->type == "table")
        type = "table";

      mapping(string:mixed) style_rec =
        ([ "field_def_id" : field_def_id,
           "type"         : type,
           "name"         : style_entry->name,
           "label_id"     : set_label(0, style_entry->label),
           "style_extref" : style_entry->ext,
           "style_extref_group" : style_entry->ext_group,
           "css"          : style_entry->css || "",
           "merge"        : style_entry->merge ]);

      if (string from_style = style_entry->from_style)
        style_rec->from_body_style = from_style;

      styles_table()->object_create (style_rec);
    }
  }

  foreach (data_provider_fields;
           int field_def_id;
           mapping(string:mixed) dp_rec) {
    REP.FieldDef fd = field_def_by_id (field_def_id);
    REP.FieldDef provider_fd =
      dp_rec->data_field &&
      comp_def->field_def_by_name (dp_rec->data_field);

    fd->set_data_provider (provider_fd,
                           dp_rec->plugin,
                           dp_rec->key,
                           dp_rec->format);
  }

  return comp_def;
}

REP.ComponentDef create_derived_component_template_def (
  REP.ComponentDef base_comp_def,
  REP.PGVersion src_pgv,
  string src_ext_page_id,
  int src_page_item_id,
  string display_name,
  string snippet,
  mapping geometry_bounds,
  mapping(string:array(mapping(string:mixed))) override_styles)
{
  mapping(string:mixed) fields = ([
    "name"               : REP.IDS.mangle_template_name (display_name),
    "label_id"           : set_label (0, ([ "en": display_name ])),
    "geometry_snippet"   : snippet,
    "revision"           : src_pgv->get ("storage_counter"),
    "parent_id"          : base_comp_def->id(),
    "source_pgv_id"      : src_pgv->id(),
    "source_ext_page_id" : src_ext_page_id,
    "source_page_item_id": src_page_item_id,
  ]);

  if (string jpeg_raw = REP.IDS.get_snippet_jpeg_preview(snippet))
    fields["geometry_snippet_preview"] = jpeg_raw;
  if (geometry_bounds)
    fields["geometry_snippet_bounds"] = geometry_bounds;

  REP.ComponentDef derived_def = copy_component_def (base_comp_def, fields,
                                                     override_styles);
  base_comp_def->clear_derived_defs_cache();
  REP.get_print_module()->clear_cached_tmpl_ids();
  return derived_def;
}

//! Deep copies an existing @[REP.ComponentDef], including associated
//! @[REP.FieldDef]:s, and returns the copy. If @[fields] is specified,
//! it is passed on to set_fields() in the newly created
//! @[REP.Component] instance.
REP.ComponentDef copy_component_def (REP.ComponentDef src_comp_def,
  void|mapping(string:mixed) fields,
  void|mapping(string:array(mapping(string:mixed))) override_styles)
{
  REP.ComponentDef new_comp_def = component_defs_table()->create_infant();
  new_comp_def->verbatim_copy (src_comp_def);

  if (fields)
    new_comp_def->set_fields (fields);

  component_defs_table()->low_object_create (new_comp_def);

  foreach (src_comp_def->get_field_defs(), REP.FieldDef field_def) {
    copy_field_def (field_def, new_comp_def->id(), 0, 0,
                    override_styles && override_styles[field_def->get_name()]);
  }

  return new_comp_def;
}

//! Deep copies an existing @[REP.FielDef], including associated
//! parameters and styles, and returns the copy.
//!
//! @param fields
//!   If specified, it is passed on to set_fields() in the newly
//!   created @[REP.FieldDef] instance.
//!
//! @param params
//!   If specified, the provided params will be used in the new field
//!   def, rather than those from @[src_field_def].
//!
//! @param styles
//!   If specified, the provided styles will be used in the new field
//!   def, rather than those from @[src_field_def]. An exception to this
//!   are base definition styles declared with the "merge" attribute that
//!   will be added regardless, though with precedence given to the custom
//!   style.
REP.FieldDef copy_field_def (REP.FieldDef src_field_def,
                             int component_def_id,
                             void|mapping(string:mixed) fields,
                             void|mapping(string:mixed) params,
                             void|array(mapping(string:mixed)) styles)
{
  mapping(string:mixed) init_rec = ([]);
  // Fields we need to be able to instantiate the infant.
  init_rec->component_def_id = component_def_id;
  init_rec->type = src_field_def->get_type();

  REP.FieldDef new_field_def =
    field_defs_table()->create_infant (init_rec);

  new_field_def->verbatim_copy (src_field_def, ([ "component_def_id": 1]));

  if (fields)
    new_field_def->set_fields (fields - ([ "component_def_id": 1 ]));

  new_field_def = field_defs_table()->low_object_create (new_field_def);
  int new_def_id = new_field_def->id();

  mapping(string:mixed) new_params;
  if (params)
    new_params = params;
  else
    new_params = src_field_def->get_raw_parameters();

  if (new_params && sizeof (new_params))
    set_lde_parameters (new_params, "field", new_def_id);

  array(REP.FieldDef) base_styles =
    styles_table()->object_select("field_def_id = " + src_field_def->id());
  array(mapping(string:mixed)) new_styles;
  if (styles) {
    new_styles = styles;
    new_styles->field_def_id = new_def_id;

    //  Merge any styles from the base definition where there isn't any
    //  definition already provided by the caller. Build a lookup table for
    //  the new styles so we can quickly determine if an override is present.
    multiset new_style_keys =
      (multiset) map(new_styles, lambda(mapping st) {
                                   return st->type + "|" + st->name;
                                 });
    foreach (base_styles, REP.FieldStyle base_style) {
      if (base_style->get_merge()) {
        string base_style_key =
          base_style->get("type") + "|" + base_style->get_name();
        if (!new_style_keys[base_style_key]) {
          //  This style specifies merging and has no override
          mapping(string:mixed) style_rec =
            base_style->get_rec() - ({ "style_id" });
          style_rec->field_def_id = new_def_id;
          new_styles += ({ style_rec });
        }
      }
    }
  } else {
    new_styles = ({});
    foreach (base_styles, REP.FieldStyle base_style) {
      mapping(string:mixed) style_rec =
        base_style->get_rec() - ({ "style_id" });
      style_rec->field_def_id = new_def_id;
      new_styles += ({ style_rec });
    }
  }

  foreach (new_styles, mapping(string:mixed) style_rec)
    styles_table()->object_create (style_rec);

  return new_field_def;
}

//! Bumps the revision number of the lde definition with prefix
//! @[prefix] (zero for the default definition), setting its current
//! file path to @[file_path].
int bump_lde_definition (void|string pub_prefix, string file_path)
{
  string nice_prefix = pub_prefix || "";
  string config_rev_counter_name = "lde_conf_rev:" + nice_prefix;
  REP.Counter counter = REP.get_counter (config_rev_counter_name);

  // Compat code for devel/demo 5.0 installations. Set the new
  // counter's min value to the maximum value of all old counters to
  // prevent any revision numbers (in StoryTemplateDefs /
  // ComponentDefs) from decreasing.
  Sql.Sql db = REP.get_db();
  array(mapping(string:mixed)) res =
    db->query ("SELECT MAX(pos) AS maxpos FROM counters "
               "WHERE name LIKE 'lde_tmpl_def_rev:%' OR "
               "      name LIKE 'lde_comp_def_rev:%'");

  if (int maxpos = (int)res[0]->maxpos) {
    counter->set_min (maxpos);
  }

  int revision = counter->get();

  REP.CachedTable tbl = lde_defs_table();

  array(mapping(string:mixed)) recs =
    tbl->cached_select ("prefix = '" + tbl->quote (nice_prefix) + "'");

  ASSERT_IF_DEBUG (sizeof (recs) <= 1);

  mapping(string:mixed) rec;
  if (sizeof (recs)) {
    rec = recs[0];
    rec->filepath = file_path;
    rec->revision = revision;
  } else {
    rec = ([ "prefix"    : nice_prefix,
             "filepath"  : file_path,
             "revision"  : revision ]);
  }

  tbl->cached_insert_or_update (rec);

  REP.get_print_module()->clear_current_lde_def_rev (nice_prefix);

  return revision;
}

//! Returns the revision of the current LDE definition for the
//! publication @[pub], or from the default definition if @[pub] is
//! zero. Returns zero if no LDE definition exists for the publication.
int current_lde_def_rev (void|REP.Publication pub)
{
  string nice_prefix = (pub && pub->get ("prefix")) || "";

  REP.PrintDBModule print_db = REP.get_print_module();

  if (mapping m = print_db->current_lde_def_revs) {
    int cached_rev = m[nice_prefix];
    if (!zero_type (cached_rev)) {
      return cached_rev;
    }
  }

  REP.CachedTable tbl = lde_defs_table();

  array(mapping(string:mixed)) recs =
    tbl->cached_select ("prefix = '" + tbl->quote (nice_prefix) + "'");

  ASSERT_IF_DEBUG (sizeof (recs) <= 1);

  int rev;
  if (sizeof (recs))
    rev = recs[0]->revision;

  if (!print_db->current_lde_def_revs) print_db->current_lde_def_revs = ([]);
  return print_db->current_lde_def_revs[nice_prefix] = rev;
}

array(REP.StoryTemplateDef)
derived_story_template_defs_for_publication (int(0..0)|REP.Publication pub,
                                             void|int include_old_pgvs)
//! Returns all derived story template defs that are extracted from a
//! current @[REP.PGVersion].
//!
//! @param pub
//!   If specified, the selection is restricted to that @[REP.Publication].
//!
//! @param include_old_pgvs
//!   If nonzero, templates derived from non-current pgvs will be
//!   returned. (Currently used by
//!   REP.LDE.ConfigParser.save_definitions().)
//!
//! Note: derived defs that have a non-current parent template def
//! will also be returned, so such filtering needs to be performed
//! separately.
{
  return derived_template_defs_for_publication (story_tmpl_defs_table(),
                                                pub,
                                                include_old_pgvs);
}

array(REP.ComponentDef)
derived_component_defs_for_publication (int(0..0)|REP.Publication pub,
                                             void|int include_old_pgvs)
//! Returns all derived component defs that are extracted from a
//! current @[REP.PGVersion].
//!
//! @param pub
//!   If specified, the selection is restricted to that @[REP.Publication].
//!
//! @param include_old_pgvs
//!   If nonzero, templates derived from non-current pgvs will be
//!   returned. (Currently used by
//!   REP.LDE.ConfigParser.save_definitions().)
//!
//! Note: derived defs that have a non-current parent template def
//! will also be returned, so such filtering needs to be performed
//! separately.
{
  return derived_template_defs_for_publication (component_defs_table(),
                                                pub,
                                                include_old_pgvs);
}

protected array(REP.TemplateDef)
derived_template_defs_for_publication (REP.DBTable tbl,
                                       int(0..0)|REP.Publication pub,
                                       void|int include_old_pgvs)
{
  array(REP.Edition) tmpl_editions = ({});
  // If no "pub" argument is given it means the default publication,
  // so we'll have to loop over the templates in all publications.
  foreach (pub ? ({ pub }) : list_publications(), REP.Publication p) {
    if (!p->get("is_deleted"))
      tmpl_editions += editions_in_publication (p, 1);
  }

  // Collect the storage counters of all page versions from
  // non-deleted page slots in non-deleted template issues.  Might
  // look somewhat heavy compared to the older SQL-only query, but
  // we need this logic to avoid template documents that are
  // assigned to deleted page slots and so on. Besides, we're only
  // looping through template editions and the result is cached.
  multiset(int) tmpl_pgv_storage_cnt = (<>);
  foreach (tmpl_editions, REP.Edition ed) {
    foreach (ed->page_slots(), REP.PageSlot slot) {
      if (REP.Page page = slot->page()) {
        REP.PGVersion cur_pgv = page->pgv();

        array(REP.PGVersion) process_pgvs = ({});
        if (include_old_pgvs) {
          process_pgvs = pgvs_by_pg_id (cur_pgv->get ("page_group_id"));
        } else {
          if (cur_pgv->get ("markup_error")) {
            // Find the most current PGVersion that doesn't have markup errors.

            // First we'll try with a limit of 5 to avoid getting
            // (potentially) lots of unnecessary PGVersions. If the version
            // we're looking for isn't found, we'll retry with a limit of 0
            // (unlimited).
            constant first_try_limit = 5;
          find_latest_pgv:
            for (int limit = first_try_limit;
                 limit >= 0;
                 limit -= first_try_limit) {
              int got_pgvs;
              foreach (cur_pgv->all_versions (limit),
                       REP.PGVersion old_pgv) {
                got_pgvs++;
                if (old_pgv->is_valid_file (REP.Storage.LAYOUT_FILE) &&
                    !old_pgv->get ("markup_error")) {
                  process_pgvs = ({ old_pgv });
                  break find_latest_pgv;
                }
              }
              if (got_pgvs < first_try_limit)
                break;
            }
          } else {
            process_pgvs = ({ cur_pgv });
          }
        }

        foreach (process_pgvs, REP.PGVersion pgv)
          tmpl_pgv_storage_cnt[pgv->get ("storage_counter")] = 1;
      }
    }
  }

  // Note: derived templates use the storage counter of the
  // PGVersion they got created from as their revision.
  array(REP.TemplateDef) derived_defs = ({});
  string sql_publ = objectp(pub) ? tbl->quote(pub->get ("prefix")) : "";
  foreach ((array)tmpl_pgv_storage_cnt / 200.0, array(int) chunk) {
    derived_defs += tbl->
      object_select ("publication_prefix = '" + sql_publ + "' AND "
                     "parent_id <> 0 AND "
                     "revision IN (" + (((array(string))chunk) * ",") + ")");
  }
  return derived_defs;
}

array(REP.StoryTemplateDef)
story_template_defs_for_publication_low (int(0..0)|REP.Publication pub)
//! Returns the most recent versions of all story templates for the
//! given publication, or from the default definition if @[pub] is
//! zero.
{
  return template_defs_for_publication_low (story_tmpl_defs_table(), pub);
}

protected array(REP.TemplateDef)
template_defs_for_publication_low (REP.DBTable tbl,
                                   int(0..0)|REP.Publication pub)
{
  string pub_prefix = objectp(pub) ? pub->get("prefix") : "";
  string sql_publ = tbl->quote (pub_prefix);

  int lde_def_rev = current_lde_def_rev (pub);
  string cache_key = tbl->table + ":" + (pub_prefix || "") + ":" +
    (string)lde_def_rev;

  array(int) ids = REP.get_print_module()->cached_tmpl_ids_for_key (cache_key);

  if (!ids) {
    array(int) base_ids = tbl->
      select1 (tbl->id_col,
               "publication_prefix = '" + sql_publ + "' AND "
               "revision = '" + (string)lde_def_rev + "' AND "
               "parent_id = 0");

    array(REP.TemplateDef) derived_defs =
      derived_template_defs_for_publication (tbl, pub);

    derived_defs = filter (derived_defs,
                           lambda (REP.TemplateDef def)
                           {
                             return has_value (base_ids,
                                               def->get ("parent_id"));
                           });

    // Perhaps it seems kind of strange to get the objects, extract
    // the ids and then do object_get_multi() below, but we need to
    // put the ids into the cache. The objects will be instantiated
    // regardless, so it isn't that much loss.
    array(int) derived_ids = derived_defs->id();

    ids = base_ids + derived_ids;
    REP.get_print_module()->set_cached_tmpl_ids_for_key (cache_key, ids);
  }

  return tbl->object_get_multi (ids);
}


array(REP.StoryTemplateDef)
story_template_defs_for_publication(int(0..0)|REP.Publication pub)
//! Returns the most recent versions of all story templates for the
//! given publication @[pub] including inherited global ones that
//! aren't overridden.
{
  array(REP.StoryTemplateDef) res = ({ });
  mapping(string:int) name_used = ([ ]);
  foreach ((pub ? story_template_defs_for_publication_low(pub) : ({ }) ) +
           story_template_defs_for_publication_low(0),
           REP.StoryTemplateDef tmpl_def) {
    //  For global story templates there is an option to limit their
    //  visibility by adding a property that list all the valid publication
    //  prefixes.
    if (pub && !tmpl_def->check_visibility_in_publication(pub))
      continue;

    string name = tmpl_def->get_name();
    if (!name_used[name]) {
      name_used[name] = 1;
      res += ({ tmpl_def });
    }
  }
  return res;
}


mapping ext_tmpl_defs_for_publ(REP.Publication pub,
                               void|int(0..1) exclude_derived,
                               void|int(0..1) exclude_ad_hoc,
                               void|REP.PGVersion pgv_context)
//! Returns a mapping structure with the most recent template definitions
//! available in the given publication @[pub], including default definitions
//! that are inherited by all publications. The structure is intended for
//! delivery to InDesign (both EP Connector and Server) where it's used to
//! check template issues for markup errors.
//!
//! For more info on @[pgv_context], see StoryTemplateDef.ext_tmpl_def.
{
  ASSERT_IF_DEBUG(pub);
  mapping(string:REP.StoryTemplateDef) story_tmpl_defs = ([ ]);

  foreach (story_template_defs_for_publication(pub),
           REP.StoryTemplateDef tmpl_def) {
    if (exclude_derived && tmpl_def->is_derived()) continue;
    if (exclude_ad_hoc && tmpl_def->is_ad_hoc()) continue;
    story_tmpl_defs[tmpl_def->get_name()] = tmpl_def;
  }

  mapping(string:REP.ComponentDef) comp_defs = ([]);
  foreach (component_defs_for_publication (pub),
           REP.ComponentDef comp_def) {
    //  We currently have no client-side use of auto-created components,
    //  i.e. those that are added in derived story templates, since we'll
    //  only output a minimal record below anyway.
    if (comp_def->is_auto_created())
      continue;

    if (exclude_derived && comp_def->is_derived()) continue;
    if (exclude_ad_hoc && comp_def->is_ad_hoc()) continue;
    comp_defs[comp_def->get_name()] = comp_def;
  }

  mapping(string:mapping(string:mixed)) ext_story_tmpl_defs =
    map (story_tmpl_defs,
         lambda (REP.StoryTemplateDef tmpl_def)
         {
           //  Get template without internal fields (e.g. "__status"),
           //  and minimize fields for derived defs to reduce size.
           int is_derived = tmpl_def->is_derived();
           return tmpl_def->ext_tmpl_def(1, pgv_context, is_derived);
         });

  mapping(string:mapping(string:mixed)) ext_comp_defs =
    map (comp_defs,
         lambda (REP.ComponentDef comp_def)
         {
           //  Only produce a minimal structure that contains component def
           //  fields that roxen.js currently needs.
           return comp_def->ext_comp_def (1, pgv_context, 1);
         });

  //  List all known publication prefixes so we can validate prefix visibility
  //  lists in story designators.
  array(string) all_prefixes = list_publications()->get("prefix");

  int is_tmpl = pgv_context && pgv_context->edition()->is_template();

  return ([ "story_tmpl_defs":      ext_story_tmpl_defs,
            "component_defs":       ext_comp_defs,
            "publication_prefixes": all_prefixes,
            "is_template_edition" : is_tmpl ]);
}

array(REP.ComponentDef)
component_defs_for_publication_low (int(0..0)|REP.Publication pub)
//! Returns the most recent versions of all component definitions for
//! the given publication, or from the default definition if @[pub] is
//! zero.
{
  return template_defs_for_publication_low (component_defs_table(), pub);
}

array(REP.ComponentDef)
component_defs_for_publication(int(0..0)|REP.Publication pub)
//! Returns the most recent versions of all component definitions for
//! the given publication @[pub] including inherited global ones
//! that aren't overridden, but excluding internal (orphaned) definitions.
{
  array(REP.ComponentDef) res = ({ });
  mapping(int:mapping(string:int)) name_used = ([ ]);
  foreach (({ pub ? component_defs_for_publication_low(pub) : ({ }),
              component_defs_for_publication_low(0) });
           int i;
           array(REP.ComponentDef) comp_defs) {
    name_used[i] = ([]);
    foreach (comp_defs, REP.ComponentDef comp_def) {
      // Exclude internal component definitions added to handle extra
      // fields. Also exclude defs from the global definition that have
      // been overridden in the publication-specific
      // definition. However, we'll still allow multiple components
      // with the same name/identifier as long as they all come from
      // either the global definition level or the
      // publication-specific level. Multiple components with the same
      // name may be the result of story templates with style
      // extraction enabled, for example (where the derived component
      // doesn't really have a specific name, it's just a copy of the
      // base component but with different style definitions).
      string name = comp_def->get_name();
      if (!(has_prefix(name, "internal-") || (i && name_used[0][name]))) {
        name_used[i][name] = 1;
        res += ({ comp_def });
      }
    }
  }
  return res;
}

REP.ComponentDef
component_def_for_publication_low(int(0..0)|REP.Publication pub,
                                  string comp_name,
                                  int|void comp_revision)
//!  Fetches a component definition by name for the given publication, or
//!  from the default definition if publication isn't given. A specific
//!  revision number may be provided; if left out the most recent definition
//!  is returned.
//!
//!  Note that individual component definitions are normally accessed via
//!  their owning story template, i.e. @[StoryTemplateDef.get_component_defs].
//!  It is only for "orphaned" components that no longer can be resolved
//!  through the story's current template that this query is necessary.
{
  string pub_prefix = objectp(pub) ? pub->get("prefix") : "";
  REP.ObjectTable tbl = component_defs_table();
  if (comp_revision) {
    //  Direct access using publication, name and revision
    array(REP.ComponentDef) res =
      tbl->object_select("publication_prefix = '" +
                         tbl->quote(pub_prefix) + "' "
                         "AND name = '" + tbl->quote(comp_name) + "' "
                         "AND revision = " + comp_revision);
    if (sizeof (res))
      return res[0];
  } else {
    foreach (component_defs_for_publication_low (pub), REP.ComponentDef comp)
      if (comp->get_name() == comp_name)
        return comp;
  }

  return 0;
}

REP.ComponentDef component_def_for_publication (REP.Publication pub,
                                                string comp_name,
                                                int|void comp_revision,
                                                void|REP.OnError on_not_found)
//!  Fetches a component definition by name for the given publication
//!  with fallback to the default definition if it isn't found in the
//!  given one. A specific revision number may be provided; if left
//!  out the most recent definition is returned.
//!
//!  Note that individual component definitions are normally accessed via
//!  their owning story template, i.e. @[StoryTemplateDef.get_component_defs].
//!  It is only for "orphaned" components that no longer can be resolved
//!  through the story's current template that this query is necessary.
{
  ASSERT_IF_DEBUG(pub);
  return
    component_def_for_publication_low (pub, comp_name, comp_revision) ||
    component_def_for_publication_low (0, comp_name, comp_revision) ||
    REP.raise_err (on_not_found,
                   "No component %O%s in publication %O.\n",
                   comp_name,
                   comp_revision ? " with revision " + comp_revision : "",
                   pub->get ("prefix"));
}


protected REP.ComponentDef get_shared_orphan_component_def(REP.Publication pub)
//! Returns (and creates if not available) the orphan component definition
//! which is used to add extra fields that don't belong in any other
//! definition.
{
  //  The "internal-" prefix is an indicator we can filter on later. The
  //  revision is hard-coded to 1.
  string orphan_def_name = "internal-orphan-comp-def";
  REP.ComponentDef res =
    component_def_for_publication_low(pub, orphan_def_name, 1);
  if (!res) {
    //  Add component definition
    mapping orphan_fields = ([ "name"   : orphan_def_name,
                               "label"  : ([ "en" : "Orphan Field Def" ]),
                               "class"  : "rep.text.internal",
                               "fields" : ({ }) ]);
    res =
      create_lde_component_def(pub->get("prefix"), orphan_fields, 1,
                               REP.RETURN_ZERO);
  }
  return res;
}


REP.FieldDef add_orphan_field_def(REP.Publication pub, mapping field_label,
                                  string field_type)
{
  //  Add field def to orphan component def
  REP.ComponentDef orphan_def = get_shared_orphan_component_def(pub);
  mapping(string:mixed) field_rec =
    ([ "component_def_id" : orphan_def->id(),
       "name"             : "internal-" + Standards.UUID.make_version4()->str(),
       "label_id"         : set_label(0, field_label),
       "type"             : field_type ]);
  int field_def_id = field_defs_table()->object_create(field_rec)->id();
  orphan_def->invalidate_field_defs();
  return field_def_by_id(field_def_id, REP.RETURN_ZERO);
}


REP.FieldDef field_def_factory (REP.ObjectTable tbl, int new_rec,
                                mapping(string:mixed) rec)
// Registered as DBObject factory callback in story_items_table().
{
  // Index constants in REP.LDE through -> instead of . to work around
  // cyclic resolver problems.
  program obj_prog = REP.LDE->field_type_classes[rec->type];
  if (!obj_prog) {
    ASSERT_IF_DEBUG (REP.LDE->field_to_mime_type[rec->type /*%O*/], rec->type);
    obj_prog = REP.FieldDef;
  }
  return obj_prog (!new_rec && rec, tbl);
}

REP.FieldDef field_def_by_id(int field_def_id,
                             void|REP.OnError on_rec_not_found)
{
  REP.ObjectTable tbl = field_defs_table();
  return tbl->object_get(field_def_id, on_rec_not_found);
}

REP.ComponentDef component_def_by_id(int component_def_id,
                                     void|REP.OnError on_rec_not_found)
{
  REP.ObjectTable tbl = component_defs_table();
  return tbl->object_get(component_def_id, on_rec_not_found);
}

REP.ComponentAreaDef
component_area_def_by_id(int component_area_def_id,
                         void|REP.OnError on_rec_not_found)
{
  REP.ObjectTable tbl = component_area_defs_table();
  return tbl->object_get(component_area_def_id, on_rec_not_found);
}

REP.StoryTemplateDef story_template_def_by_id(int story_tmpl_def_id,
                                              void|REP.OnError on_rec_not_found)
{
  REP.ObjectTable tbl = story_tmpl_defs_table();
  return tbl->object_get(story_tmpl_def_id, on_rec_not_found);
}

REP.StoryTemplateDef story_template_def_for_publication_low(
  int|REP.Publication pub,
  string tmpl_id,
  int|void tmpl_revision,
  int|void base_def_only)
//!  Fetches a story template by name (which is a ID string) for the
//!  given publication, or from the default definition if publication
//!  isn't given. A specific revision number may be provided; if left
//!  out the most recent definition is returned (which may be stale,
//!  i.e. not present in the most recent definition.)
{
  string pub_prefix = "";
  if (intp(pub) && pub)
    pub = publication_by_id(pub);
  if (objectp(pub))
    pub_prefix = pub->get("prefix");

  REP.ObjectTable tbl = story_tmpl_defs_table();
  array(REP.StoryTemplateDef) res;
  if (tmpl_revision) {
    //  Direct access using publication, id and revision
    res =
      tbl->object_select("publication_prefix = '" + tbl->quote(pub_prefix) + "' "
                         "AND name = '" + tbl->quote(tmpl_id) + "' "
                         "AND revision = " + tmpl_revision +
                         (base_def_only ? " AND parent_id = 0" : ""));
  } else {
    // Use cached objects. This saves an SQL query per call to this
    // function (and it is called frequently at times).
    res = filter (story_template_defs_for_publication_low (pub),
                  lambda (REP.StoryTemplateDef def) {
                    if (base_def_only && def->is_derived())
                      return 0;

                    return def->get_name() == tmpl_id;
                  });
  }
  return sizeof(res) && res[0];
}

REP.StoryTemplateDef story_template_def_for_publication(
  int|REP.Publication pub,
  string tmpl_id,
  int|void tmpl_revision,
  void|REP.OnError on_rec_not_found)
//!  Fetches a story template by name (which is a ID string) for the
//!  given publication with fallback to the default definition if publication
//!  isn't given. A specific revision number may be provided; if left
//!  out the most recent definition is returned (which may be stale,
//!  i.e. not present in the most recent definition.)
{
  //  Fetch data with fallback to global definitions. Any errors will be
  //  handled according to caller's wishes below.
  //  in low-level function
  REP.StoryTemplateDef tmpl_def =
    story_template_def_for_publication_low(pub, tmpl_id, tmpl_revision) ||
    story_template_def_for_publication_low(0, tmpl_id, tmpl_revision);
  if (!tmpl_def) {
    if (tmpl_revision) {
      return REP.raise_err(on_rec_not_found,
                           "Story template %q version %d not found.\n",
                           tmpl_id, tmpl_revision);
    } else {
      return REP.raise_err(on_rec_not_found,
                           "Story template %q not found.\n", tmpl_id);
    }
  }
  return tmpl_def;
}

REP.StoryTemplateDef
story_template_base_def_for_publication (int|REP.Publication pub,
                                         string tmpl_id,
                                         void|int tmpl_revision,
                                         void|REP.OnError on_rec_not_found)
//! Identical to @[story_template_def_for_publication], except that it
//! only returns base definitions (i.e. non-derived).
{
  //  Fetch data with fallback to global definitions. Any errors will be
  //  handled according to caller's wishes below.
  //  in low-level function
  REP.StoryTemplateDef tmpl_def =
    story_template_def_for_publication_low(pub, tmpl_id, tmpl_revision, 1) ||
    story_template_def_for_publication_low(0, tmpl_id, tmpl_revision, 1);
  if (!tmpl_def) {
    if (tmpl_revision) {
      return REP.raise_err(on_rec_not_found,
                           "Story template %q version %d not found.\n",
                           tmpl_id, tmpl_revision);
    } else {
      return REP.raise_err(on_rec_not_found,
                           "Story template %q not found.\n", tmpl_id);
    }
  }
  return tmpl_def;
}

array(REP.StoryTemplateDef) derived_story_template_defs_by_base_def (
  REP.StoryTemplateDef base_def)
{
  REP.ObjectTable tbl = story_tmpl_defs_table();
  return tbl->object_select ("parent_id = " + base_def->id());
}

//
// Story stuff below
//

array(REP.Story) list_stories(void|REP.Edition|REP.PGVersion selection,
                              void|int deleted,
                              void|int start_timestamp,
            void|int end_timestamp)
//! Lists stories based on a selection.
//!
//! @param selection
//!   If the selection is a @[REP.Edition], all stories that have
//!   appearances in that edition are returned. If the selection is a
//!   @[REP.PGVersion], all stories that have appearances on that page
//!   group are returned. (Both cases disregard the stories' home
//!   publication).
//!
//! @param deleted
//!   If non-zero, deleted stories are returned instead of non-deleted.
//!
//! @param start_timestamp
//!   If non-zero, only stories modified after this time will be listed. Value
//!   is inclusive so stories modified exactly at this time are also included.
//!
//! @param end_timestamp
//!   If non-zero, only stories not modified after this time will be listed.
//!   Value is exclusive so stories modified exactly at this time will not be
//!   included.
{
  REP.Edition edition;

  array(string) where = ({ (deleted ? "delete_at > 0" : "delete_at = 0") });
  array(string) refs = ({ });
  array(REP.Story) ret;

  if (!selection) {
    werror ("REP.DB.list_stories(): Warning - listing ALL stories.\n");
  }
  else if (selection->is_rep_edition) {
    edition = selection;

    where += ({ "edition_id = '" + edition->id() + "'" });
    refs += ({ "INNER JOIN story_apps USING (story_id)" });
  } else {
    ASSERT_IF_DEBUG (selection->is_rep_pgv);
    ret = selection->stories (!!deleted, 1);
  }

  if (start_timestamp) {
    where += ({ sprintf ("IFNULL(stories.date_modified, stories.date_created) "
                         ">= FROM_UNIXTIME(%d)", start_timestamp) });
  }

  if (end_timestamp) {
    where += ({ sprintf ("IFNULL(stories.date_modified, stories.date_created) "
       "< FROM_UNIXTIME(%d)", end_timestamp) });
  }

  if (!ret)
    ret = stories_table()->object_select(where * " AND ",
                                         refs * " ");

  // FIXME? Stories can currently have appearances in editions
  // belonging to different publications.
  //
  // ASSERT_IF_DEBUG (!sizeof (ret) || !edition ||
  // 		   equal (Array.uniq (ret->get ("publication_id")),
  // 			  ({ edition->publication()->id() })));

  return ret;
}

array(REP.Story) list_stack_stories (REP.Publication|REP.Edition selection,
                                     void|int deleted,
                                     void|int start_timestamp)
//! Lists stories that have appearances in a publication or edition
//! stack (regardless of the stories' home publication in the case of
//! an edition stack.)
//!
//! @param selection
//!   If the selection is a @[REP.Publication], all stories on the
//!   publication stack are returned. If the selection is a
//!   @[REP.Edition], all stories on the edition stack are returned.
//!
//! @param deleted
//!   If non-zero, deleted stories are returned instead of non-deleted.
{
  ASSERT_IF_DEBUG (selection->is_rep_publication || selection->is_rep_edition);
  return selection->stack_stories (deleted, start_timestamp);
}

int|array(REP.Story) list_stories_by_assignee_low (int|string|RequestID id_spec,
                                                   void|REP.Edition edition,
                                                   void|REP.Publication publ,
                                                   void|int deleted,
                                                   void|int start_timestamp,
                                                   void|REP.OnError user_not_found,
                                                   void|int count_only)
{
  int user_id = REP.get_session()->mac->id_get_id (id_spec);

  if (!user_id)
    return REP.raise_err(user_not_found, "No user found from %O.\n", id_spec);

  REP.ObjectTable tbl = stories_table();

  array(string) where = ({
    "assignments.user_id = " + user_id,
    ("assignments.entity_type = '" +
     tbl->quote (REP.Types.assignment_type_story) + "'"),
  });

  array(string) joins = ({
    "INNER JOIN assignments ON stories.story_id = assignments.entity_id "
  });

  if (edition) {
    where += ({ "story_apps.edition_id = " + edition->id() });
    joins += ({ "INNER JOIN story_apps USING (story_id) " });
  }

  if (deleted) {
    where += ({ "delete_at > 0" });
  } else {
    where += ({ "delete_at = 0" });
  }

  if (start_timestamp) {
    where += ({ sprintf ("IFNULL(stories.date_modified, stories.date_created) "
                         " >= FROM_UNIXTIME(%d)", start_timestamp) });
  }

  if (count_only) {
    //  We avoid object_select since these items needn't be brought into RAM
    array rec = tbl->select1("COUNT(*) AS num", where * " AND ", joins * " ");
    return rec && rec[0];
  }

  array(REP.Story) stories =
    tbl->object_select(where * " AND ",
                       joins * " ");

  return stories;
}

array(REP.Story) list_stories_by_assignee (int|string|RequestID id_spec,
                                           void|REP.Edition edition,
                                           void|REP.Publication publ,
                                           void|int deleted,
                                           void|int start_timestamp,
                                           void|REP.OnError user_not_found)
{
  return list_stories_by_assignee_low(id_spec, edition, publ, deleted,
                                      start_timestamp, user_not_found, 0);
}

int count_stories_by_assignee(int|string|RequestID id_spec,
                                           void|REP.Edition edition,
                                           void|REP.Publication publ,
                                           void|int deleted,
                                           void|int start_timestamp,
                                           void|REP.OnError user_not_found)
{
  return list_stories_by_assignee_low(id_spec, edition, publ, deleted,
                                      start_timestamp, user_not_found, 1);
}

REP.Story story_by_id (int story_id, void|REP.OnError on_rec_not_found)
//! Returns an existing story object based on the story id. It is an
//! error if there is no story with the given id. The value of
//! @[on_rec_not_found] determines how this error is handled by this
//! method.
//!
//! @returns
//!        An existing story object or 0, if no story with the given id exists.
//!
//! @seealso
//!   @[story_by_uuid()]
{
  REP.ObjectTable tbl = stories_table();
  return tbl->object_get(story_id, on_rec_not_found);
}

REP.Story story_by_uuid (string story_uuid, void|REP.OnError on_rec_not_found)
//! Returns an existing story object based on the story uuid. It is an
//! error if there is no story with the given uuid. The value of
//! @[on_rec_not_found] determines how this error is handled by this
//! method.
//!
//! @returns
//!        An existing story object or 0, if no story with the given id exists.
//!
//! @seealso
//!   @[story_by_id()]
{
  REP.ObjectTable tbl = stories_table();
  REP.Story story = story_by_id (rec_id_by_uuid (tbl, story_uuid),
                                 REP.RETURN_ZERO);

  if (story)
    return story;

  return REP.raise_err(on_rec_not_found,
                       "Record uuid '%s' not found in %s.\n",
                       story_uuid, tbl->table);
}

array(REP.Story) get_story_branch_collection(REP.Story story)
{
  //  A collection of branched stories will all point to the same parent
  //  story ID. If the given story has a parent ID that value is the one
  //  to use (since parent pointers should never be chained multiple
  //  levels); if none is found we assume the story itself is the parent of
  //  the collection.
  //
  //  Note that it's possible that the top-level story is deleted or
  //  inaccessible for other reasons, but its ID is a valid collection
  //  identifier regardless.
  //
  //  Also note that the result always includes the given story unless it's
  //  flagged as deleted. The result is not sorted.
  int main_story_id = story->get("parent") || story->id();

  REP.ObjectTable tbl = stories_table();
  array(int) branch_ids =
    tbl->select1("story_id", ({ "parent = " + main_story_id }) ) +
    ({ main_story_id });
  return filter(map(branch_ids, REP.DB.story_by_id, REP.RETURN_ZERO),
                lambda(REP.Story s) {
                  return !s->get("delete_at");
                });
}

string get_ext_id_for_story_branch(string base_ext_id, REP.Story story)
{
  //  Takes an external ID and appends a suffix to make it specific to a
  //  given story instance (which typically will be the branched story in
  //  a Z/E sub-edition). If the story is branched repeatedly we don't
  //  want to grow the external ID so any earlier suffix is removed first.
  return base_ext_id && ((base_ext_id / "|ze:")[0] + "|ze:" + story->id());
}

void set_story_branch_parent(REP.Story branch_story, REP.Story parent_story,
                             void|mapping(string:int) map_id_output)
{
  //  It's not valid to have a story reference itself
  if (branch_story == parent_story)
    return;

  //  Avoid creating a chain of branch IDs by reusing the parent's branch
  //  pointer if available.
  int top_branch_story_id = parent_story->get("parent") || parent_story->id();
  branch_story->set_fields( ([ "parent": top_branch_story_id ]) );

  //  If the parent story was imported from an external writer we need to
  //  construct a new set of external ID references that connect the items
  //  in the new story as well.
  //
  //  A prerequisite is that we are passed a mapping of old to new IDs for
  //  the story and the SIVs. This mapping is collected in calls to
  //  copy_story() and copy_story_item().
  if (map_id_output) {
    //  Check if any importer claims ownership of the parent story. We need
    //  this to find the correct external ID namespace.
    if (mapping importer_info =
        REP.StoryImport.get_importer_info(parent_story)) {
      string ns = importer_info->namespace;
      string origin_type = importer_info->origin_type;
      SqlTools.SqlTable uuids_table = external_ids_table();

      //  Duplicate external record for parent story
      ASSERT_IF_DEBUG(map_id_output["story:" + parent_story->id()]);
      foreach (external_ids_for_dbobj(parent_story, ns), string ext_id) {
        string branch_ext_id =
          get_ext_id_for_story_branch(ext_id, branch_story);
        array(string) branch_origin_ext_ids =
          origin_external_ids_for_dbobj(parent_story, ns);
        lookup_or_add_external_id(branch_story,
                                  ns,
                                  branch_ext_id,
                                  (sizeof(branch_origin_ext_ids) &&
                                   branch_origin_ext_ids[0]),
                                  origin_type);
      }

      //  Duplicate external records for parent story items (unversioned)
      foreach (map_id_output; string parent_si_key; int branch_si_id) {
        sscanf(parent_si_key, "si:%d", int parent_si_id);
        if (!parent_si_id)
          continue;
        mapping parent_si_rec = ([ "story_item_id": parent_si_id ]);
        mapping branch_si_rec = ([ "story_item_id": branch_si_id ]);

        foreach (external_ids_for_dbobj(parent_si_rec, ns), string si_ext_id) {
          string branch_si_ext_id =
            get_ext_id_for_story_branch(si_ext_id, branch_story);
          lookup_or_add_external_id(branch_si_rec, ns, branch_si_ext_id);
        }

        //  Restore story item "origin" field that is cleared in standard
        //  story item copying.
        if (REP.SIVersion parent_siv =
            current_siv_by_si_id(parent_si_id, REP.RETURN_ZERO)) {
          if (string parent_siv_origin = parent_siv->get_unver("origin")) {
            if (REP.SIVersion branch_siv =
                current_siv_by_si_id(branch_si_id, REP.RETURN_ZERO)) {
              branch_siv->set_unver_fields( ([ "origin": parent_siv_origin ]) );
            }
          }
        }
      }
    }
  }
}

bool unlink_story_branch(REP.Story story, bool clear_title_suffix)
{
  bool is_ok = false;

  //  Can only unlink the story that acts as the parent of a collection
  if (story->get("parent")) {
    //  Clear parent pointer and optionally title suffix
    mapping fields = ([ "parent": 0 ]);
    if (clear_title_suffix)
      fields["title_suffix"] = 0;
    story->set_fields(fields);
    is_ok = true;
  }

  //  Remove any external writer bindings by searching for such namespaces
  //  in the external ID table. This is supported also if the story wasn't
  //  part of a collection.
  if (mapping importer_info = REP.StoryImport.get_importer_info(story)) {
    if (string ns = importer_info->namespace) {
      foreach (external_ids_for_dbobj(story, ns), string story_ext_id) {
        remove_external_id(ns, story_ext_id);
      }
      foreach (story->current_items(), REP.SIVersion siv) {
        mapping si_rec = siv->get_rec_unver();
        foreach (external_ids_for_dbobj(si_rec, ns), string si_ext_id) {
          remove_external_id(ns, si_ext_id);
        }

        //  Also clear origin field
        siv->set_unver_fields( ([ "origin": UNDEFINED ]) );
      }

      is_ok = true;
    }
  }

  return is_ok;
}


REP.Story create_story (REP.Publication publ,
                        REP.StoryTemplateDef story_tmpl_def,
                        void|mapping(string:mixed) init_values,
                        void|REP.OnError on_value_error)
//! Creates a new story and returns the resulting story object.
//!
//! @param story_tmpl_def
//!   The @[REP.StoryTemplateDef] to use.
//!
//! @param init_values
//!   Initial low-level values to set in the created story.
//!
//! @param on_value_error
//!   What to do if invalid init_values were encountered.
//!
//! @seealso
//! @[copy_story]
{
  ASSERT_IF_DEBUG (publ->id());
  ASSERT_IF_DEBUG (story_tmpl_def);
  ASSERT_IF_DEBUG (story_tmpl_def->id());

  init_values = init_values || ([]);
  init_values->publication = publ;

  init_values->story_tmpl_def = story_tmpl_def;

  REP.Story story =
    stories_table()->object_create (init_values, on_value_error);
  if (!story) return 0;

  // Make a super stack appearance.
  story_apps_table()->insert ((["story_id": story->id(),
                                "edition_id": Val.null]));

  story->invalidate_appearances();
  story->publication()->invalidate_story_appearances();
  REP.get_print_module()->notify_story_apps_changed (story,
                                                     story->publication());


  mapping(string:mixed) si_ver_fields = ([]);
  if (!zero_type (init_values->user_id))
    si_ver_fields->user_id = init_values->user_id;

  foreach (story_tmpl_def->component_area_defs(),
           REP.ComponentAreaDef ca_def) {
    for (int i = 0; i < ca_def->size(); i++) {
      REP.SIVersion siv =
        create_story_item (story,
                           ca_def->component_def(),
                           ([ "component_area_def" : ca_def,
                              "comp_area_order"       : i+1 ]),
                           si_ver_fields + ([]) );
      make_siv_current (siv, 0);
    }
  }

  return story;
}

REP.Story copy_story (REP.Story src, void|REP.Publication dst_publ,
                      void|mapping(string:int) map_id_output)
//! Creates a copy of the story @[src], including its current items.
//! The copy only gets a super stack appearance - use @[add_story_app]
//! afterwards for other appearances. The copy keeps the same story
//! template as the original.
//!
//! @param src
//!   The story to copy.
//!
//! @param dst_publ
//!   The publication to create the copy in. Defaults to the same
//!   publication as @[src]. Note that the copy keeps the story
//!   template even if it doesn't exist in @[dst_publ] - it might need
//!   to be converted afterwards.
//!
//! @returns
//!   Returns the new copy.
{
  if (!dst_publ) dst_publ = src->publication();

  REP.ObjectTable tbl = stories_table();
  REP.Story story = tbl->create_infant();
  story->verbatim_copy (src, ([ "date_created" : 1 ]));

  //  We leave the "parent" field untouched so we don't accidentally create
  //  any branch relationship. The caller will have to set this separately if
  //  needed.
  story->set_fields ((["publication": dst_publ,
                       "story_uuid": Standards.UUID.make_version4()->str(),
                       "date_created": time(),
                       "user_id": REP.get_session()->user_id(),
                       "deleted": 0]));

  story = tbl->low_object_create (story);
  if (map_id_output)
    map_id_output["story:" + src->id()] = story->id();

  //  Copy assignees
  foreach (src->get_assignees(), int user_id)
    story->add_assignee(user_id, REP.RETURN_ZERO);

  //  Copy categories
  array(REP.Category) copy_categories = ({ });
  foreach (src->get_categories(1, 0), REP.Story.Category c) {
    REP.Category copy_c = REP.Category(c->get("story_category"),
                                       c->get("is_main"),
                                       c->get("priority"));
    copy_categories += ({ copy_c });
  }
  if (sizeof(copy_categories))
    story->add_categories(copy_categories, REP.RETURN_ZERO);

  // Make a super stack appearance.
  story_apps_table()->insert ((["story_id": story->id(),
                                "edition_id": Val.null]));

  foreach (src->current_items(), REP.SIVersion src_siv)
    make_siv_current (copy_story_item (src_siv, story, 0, map_id_output));

  return story;
}

void copy_stories_between_page_groups (
  REP.PGVersion src_pgv,
  REP.PGVersion dst_pgv,
  void|int deep_copy,
  void|mapping(int:int) remap_story_doc_ids,
  void|int reset_placements,
  void|int only_remapped_stories)
//! Copies all stories appearing in the page group designated by
//! @[src_pgv] to the page group for @[dst_pgv]. Does nothing if the
//! two objects belong to the same page group.
//!
//! If @[deep_copy] is set, the stories are always copied completely
//! using @[copy_story], otherwise new appearances are added to the
//! same stories if possible (i.e. if @[src_pgv] and @[dst_pgv] is in
//! the same publication).
//!
//! If @[remap_story_doc_ids] is provided (as a mapping), the
//! story_doc_ids contained in it will be remapped when story
//! appearances are copied between the page groups. This is used by
//! e.g. the IDS merge_contents operation when it needs to remap
//! story_doc_ids in the merged document due to conflicts.
//!
//! If @[reset_placements] is non-zero, any "placed" markers and
//! story_doc_ids will be reset when appearances are copied.
//!
//! By setting @[only_remapped_stories] we only copy or add new appearances
//! to stories listed in @[remap_story_doc_ids] instead of all source page
//! stories.
{
  if (src_pgv->get("page_group_id") == dst_pgv->get("page_group_id"))
    return;

  REP.Edition dst_ed = dst_pgv->edition();

  //  Disallow adding of appearances to template editions
  if (dst_ed->is_template())
    return;

  array(REP.Story) src_stories = src_pgv->stories (0, 1);

  mapping(int:REP.Story) stories_by_doc_ids = src_pgv->stories_by_doc_ids();
  mapping(REP.Story:int) doc_ids_by_story =
    mkmapping (values (stories_by_doc_ids),
               indices (stories_by_doc_ids));

  if (!deep_copy) {
    //  Despite what the defvar name says the setting affects copying between
    //  issues within as well as outside of a publication.
    deep_copy =
      src_pgv->edition() != dst_ed &&
      REP.get_print_module()->query ("deep_story_copying_between_publs");
  }

  multiset(REP.Story) dst_pgv_stories = (multiset)dst_pgv->stories (-1, 1);

  foreach (src_stories, REP.Story src) {
    int skip_unmapped = 0;
    if (only_remapped_stories && remap_story_doc_ids)
      skip_unmapped = !remap_story_doc_ids[doc_ids_by_story[src]];
    if (dst_pgv_stories[src] || skip_unmapped)
      continue;

    REP.Story story;
    if (deep_copy) {
      story = copy_story (src, dst_ed->publication());
    } else {
      story = src;
    }
    int story_doc_id = doc_ids_by_story[src];
    if (int remapped = (remap_story_doc_ids &&
                        remap_story_doc_ids[story_doc_id])) {
      story_doc_id = remapped;
    }
    if (!reset_placements && story_doc_id) {
      add_story_app (story, dst_ed, dst_pgv, 1, story_doc_id);
    } else {
      add_story_app (story, dst_ed, dst_pgv);
    }
  }
}

int add_story_app(REP.Story story,
                  REP.Edition edition,
                  void|REP.PGVersion pgv,
                  void|int is_placed,
                  void|int story_doc_id)
//! Adds a story appearance for a story, if none exists already.
//!
//! @param story
//!   The story to add the appearance for.
//!
//! @param edition
//!   The edition to add the appearance for. If unset, it will be looked
//!   up from @[pgv]. Note that template editions are not accepted for
//!   appearances.
//!
//! @param pgv
//!   The page group to add the appearance for. If unset, the
//!   appearance will be added to the edition stack of @[edition].
//!
//! @param is_placed
//!   Sets the is_placed flag in the appearance. Applicable only if
//!   @[pgv] is given.
//!
//! @param story_doc_id
//!   Sets the story_doc_id (the unique id of this story in a page file)
//!   for this appearance.
//!
//! @returns
//!   Returns the id of the story_app_parts record for the appearance,
//!   regardless whether a new one was created or not. If the edition is
//!   a template edition we exit early with @expr{0@} as the result.
//!
//! @note
//! A story is on the publication stack iff it doesn't have any
//! appearance at all.
{
  int story_id = story->id();

  if(!edition) {
    ASSERT_IF_DEBUG (pgv);
    edition = pgv->edition();
  }
  else
    ASSERT_IF_DEBUG (!pgv || pgv->edition() == edition);

  ASSERT_IF_DEBUG (!is_placed || (is_placed && pgv));
  ASSERT_IF_DEBUG (!story_doc_id || (story_doc_id && pgv));

  //  We disallow appearances in template editions. In that case we don't
  //  even bother looking up any existing appearance but exit directly.
  if (edition->is_template())
    return 0;

  SqlTools.SqlTable sa_tbl = story_apps_table();
  int story_app_id = sa_tbl->insert_or_update ((["story_id": story_id,
                                                 "edition_id": edition->id()]));

  // Remove any appearance from the publication stack.
  sa_tbl->delete ("story_id = " + story_id + " AND edition_id IS NULL");
  int publ_stack_affected = sa_tbl->get_db()->master_sql->affected_rows() > 0;

  int|Val.Null pg_id = pgv ? pgv->get ("page_group_id") : Val.null;
  mapping rec = ([ "story_app_id": story_app_id,
                   "page_group_id": pg_id ]);
  REP.CachedTable sa_parts_tbl = story_app_parts_table();
  array(int) sqlres =
    sa_parts_tbl->select1 ("sa_part_id",
                           "story_app_id = " + story_app_id +
                           " AND page_group_id " +
                           (pg_id == Val.null ? "IS NULL" : (" = " + pg_id)));

  if (sizeof (sqlres)) {
    rec["sa_part_id"] = sqlres[0];
  }

  if (pgv && !zero_type (story_doc_id)) {
    rec->story_doc_id = story_doc_id;
  }

  if (pgv && !zero_type(is_placed)) {
    rec->is_placed = is_placed;
  }

  int sa_part_id = story_app_parts_table()->cached_insert_or_update (rec);

  story->invalidate_appearances();
  if (publ_stack_affected)
    story->publication()->invalidate_story_appearances();
  edition->invalidate_story_appearances();
  if (pgv) pgv->invalidate_appearances(1);
  REP.get_print_module()->notify_story_apps_changed (story,
                                                     publ_stack_affected &&
                                                     story->publication());
  REP.get_print_module()->notify_story_apps_changed (story, edition);

  return sa_part_id;
}

REP.Story modify_story_app(int sa_part_id,
                           REP.PGVersion|REP.Edition|int(0..0) dst,
                           void|int is_placed,
                           void|REP.Story story)
//! Modifies a story appearance.
//!
//! @param sa_part_id
//! The story_app_parts.sa_part_id identifier of the appearance
//! record.
//!
//! @param dst
//! The new destination of the appearance.
//!
//! If @[dst] is a @[REP.PGVersion] then it specifies the page group
//! for the appearance, otherwise the appearance is set to the stack.
//!
//! If @[dst] is a @[REP.Edition], it sets the appearance to the stack
//! of that edition.
//!
//! If @[dst] is zero, the appearance is set to the stack without
//! changing the current edition.
//!
//! @param is_placed
//! Updates the is_placed flag in the appearance if it isn't
//! @[UNDEFINED].
//!
//! @param story
//! The story of the appearance. This is optional and may be given to
//! save a database query.
//!
//! @returns
//! Returns the story object for the appearance, i.e. @[story] if it
//! was given.
{
  ASSERT_IF_DEBUG(sa_part_id);

  if (!story)
    story = story_by_id (story_apps_table()->select1 (
                           "story_id",
                           "story_app_parts.sa_part_id = " + sa_part_id,
                           "JOIN story_app_parts USING (story_app_id)")[0]);
  else
    ASSERT_IF_DEBUG (story->story_app_parts()[sa_part_id]);

  REP.PGVersion old_pgv = story->appearance_pg (sa_part_id);
  REP.Edition old_ed = story->appearance_ed (sa_part_id);
  REP.PGVersion pgv;
  REP.Edition ed;

  Sql.Sql db = REP.get_db();

  mapping(string:mixed) sap_fields = (["sa_part_id": sa_part_id]);

  if (dst && dst->is_rep_pgv) {
    pgv = dst;
    ed = pgv->edition();
    sap_fields->page_group_id = pgv->get_unver ("page_group_id");
  } else {
    ed = dst;
    sap_fields->page_group_id = Val.null;
  }

  if (!zero_type (is_placed))
    sap_fields->is_placed = is_placed;

  int dont_update;

  if (sap_fields->page_group_id) {
    string where = "t2.sa_part_id = :sa_part_id "
      "AND story_app_parts.page_group_id = :page_group_id "
      "AND story_app_parts.story_app_id = t2.story_app_id";

    mapping(string:mixed) bindings =
      ([ "sa_part_id": sap_fields->sa_part_id,
         "page_group_id": sap_fields->page_group_id ]);

    array(mapping(string:mixed)) res =
      story_app_parts_table()->cached_select (({ where, bindings }),
                                              ", story_app_parts AS t2");

    if (sizeof (res)) // There is already an appearance with this
                      // story_app_id/page_group_id
                      // combination. Ignore the update and preserve
                      // the existing one. The appearance we tried to
                      // update will be kept as-is.
      dont_update = 1;
  }

  if (!dont_update)
    story_app_parts_table()->cached_update (sap_fields);

  if (ed) {
    db->query("UPDATE story_apps, story_app_parts "
              "   SET story_apps.edition_id=%d "
              " WHERE story_apps.story_app_id = story_app_parts.story_app_id "
              "   AND story_app_parts.sa_part_id = %d",
              ed->id(), sa_part_id);
    ed->invalidate_story_appearances();
  }

  story->invalidate_appearances();
  old_ed->invalidate_story_appearances();

  if (old_pgv != pgv) {
    if (old_pgv) old_pgv->invalidate_appearances(1);
    if (pgv) pgv->invalidate_appearances(1);
  }

  REP.get_print_module()->notify_story_apps_changed (story, old_pgv || ed);

  return story;
}

mapping(string:mixed) get_story_app_fields (int sa_part_id,
                                            void|REP.OnError on_rec_not_found)
//! Returns all fields for the edition specific story appearance with
//! the given id. Don't be destructive on the returned mapping.
{
  return story_app_parts_table()->cached_get (sa_part_id, on_rec_not_found);
}

void set_story_app_fields (mapping(string:mixed) fields)
//! Changes fields for an edition specific story appearance. Fields
//! not mentioned in @[fields] are kept. A property field can be
//! removed by specifying @[Val.null] as value.
//!
//! @[fields]->story_slot may be a @[REP.PGVersion.StorySlot] object.
//!
//! The appearance to change is specified in one of these ways:
//!
//! @ul
//! @item
//!   An integer @[fields]->sa_part_id. An entry @[fields]->story is
//!   optional but saves a db query.
//! @item
//!   Two entries @[fields]->story and @[fields]->pgv containing a
//!   @[REP.Story] and a @[REP.PGVersion] object, respectively. This
//!   changes an appearance on a page.
//! @item
//!   Two entries @[fields]->story and @[fields]->edition containing
//!   a @[REP.Story] and a @[REP.Edition] object, respectively. This
//!   changes an appearance on the edition stack.
//! @endul
//!
//! Destructively changes @[fields] to contain sa_part_id and the
//! values that actually are set in the record.
//!
//! This function can not be used to modify the story, page group, or
//! edition associations - use @[modify_story_app] for that.
{
  ASSERT_IF_DEBUG (fields->sa_part_id ||
                   (fields->story && (fields->pgv || fields->edition))
                   /*fields=%O*/, fields);
  ASSERT_IF_DEBUG (zero_type (fields->story_app_id));
  ASSERT_IF_DEBUG (zero_type (fields->page_group_id));
  ASSERT_IF_DEBUG (!fields->text_chain_uuids ||
                   multisetp (fields->text_chain_uuids));

#if 0
  if (!zero_type (fields->story_slot)) {
    if (!fields->story_slot)
      fields->story_slot = Val.null;
    else if (objectp (fields->story_slot)) {
      ASSERT_IF_DEBUG (!fields->story_slot || !fields->pgv ||
                       fields->story_slot->pgv == fields->pgv
                       /*fields=%O*/, fields);
      fields->story_slot = fields->story_slot->get_slot_id();
    }
  }
#endif

  REP.Story story = m_delete (fields, "story");

  REP.CachedTable sap_tbl = story_app_parts_table();
  TableLock lock;
  REP.PGVersion pgv;
  REP.Edition edition;

  if (fields->sa_part_id) {
    if (sizeof (fields - sap_tbl->col_types -
                (["story": "", "pgv": "", "edition": ""])))
      // Must lock the table to guarantee atomicity in sap_tbl->update if
      // a property is changed.
      lock = TableLock ((["story_apps": READ_LOCK,
                          "story_app_parts": WRITE_LOCK]));
  }

  else {
    pgv = m_delete (fields, "pgv");
    edition = (pgv && pgv->edition()) ||
      m_delete (fields, "edition");

    lock = TableLock ((["story_apps": READ_LOCK,
                        "story_app_parts": WRITE_LOCK]));

    array(mapping(string:mixed)) recs;
    if (pgv)
      recs = sap_tbl->cached_select (
        "story_id=" + story->id() + " AND "
        "edition_id=" + edition->id() + " AND "
        "page_group_id=" + pgv->get ("page_group_id"),
        "JOIN story_apps USING (story_app_id)");
    else
      recs = sap_tbl->cached_select (
        "story_id=" + story->id() + " AND "
        "edition_id=" + edition->id() + " AND "
        "page_group_id IS NULL",
        "JOIN story_apps USING (story_app_id)");
    if (!sizeof (recs)) return;
    fields->sa_part_id = recs[0]->sa_part_id;
  }

  mapping(string:mixed) changed_fields = ([]);

check_changes: {
    mapping(string:mixed) old_fields = sap_tbl->cached_get (fields->sa_part_id);
    foreach (fields; string name; mixed val) {
      mixed old_val = old_fields[name];
      if (zero_type (old_val) ? val != Val.null : !equal (val, old_val))
        changed_fields[name] = val;
    }
    if (!sizeof (changed_fields)) {
      if (lock) destruct (lock);
      return;		 // No changes. Skip update and notifications.
    }
  }

  sap_tbl->cached_update (fields);

  if (!story || (changed_fields->story_doc_id && !pgv)) {
    array(REP.Story|REP.PGVersion) res =
      get_story_and_pgv_by_sa_part_id (fields->sa_part_id);
    story = story || res[0];
    pgv = pgv || res[1];
  }

  if (lock) destruct (lock);

  // Only need to invalidate page group appearance caches if the
  // story_doc_id changed.
  if (changed_fields->story_doc_id && pgv)
    pgv->invalidate_appearances(story && 1);

  if (story) {
    story->invalidate_appearances();
    REP.get_print_module()->notify_story_apps_changed(story);
  }
}

array(REP.Story|REP.PGVersion)
get_story_and_pgv_by_sa_part_id (int sa_part_id)
{
  Sql.Sql db = REP.get_db();

  array(mapping(string:mixed)) res =
    db->typed_query ("SELECT story_id, page_group_id "
                     "  FROM story_apps "
                     "  JOIN story_app_parts "
                     "    USING (story_app_id) "
                     " WHERE sa_part_id = :sa_part_id",
                     ([ "sa_part_id": sa_part_id ]));

  int story_id = sizeof (res) && res[0]->story_id;
  int pg_id = sizeof (res) && res[0]->page_group_id;

  REP.Story story = story_id && story_by_id (story_id);
  REP.PGVersion pgv = pg_id && current_pgv_by_pg_id (pg_id);

  return ({ story, pgv });
}

int get_story_id_by_sa_part_id (int sa_part_id)
{
  Sql.Sql db = REP.get_db();

  array(mapping(string:mixed)) res =
    db->typed_query ("SELECT story_id, page_group_id "
         "  FROM story_apps "
         "  JOIN story_app_parts "
         "    USING (story_app_id) "
         " WHERE sa_part_id = :sa_part_id",
         ([ "sa_part_id": sa_part_id ]));

  int story_id = sizeof (res) && res[0]->story_id;
  return story_id;
}

string sa_part_id_obj_path (int sa_part_id)
{
  array(REP.Story|REP.PGVersion) res =
    get_story_and_pgv_by_sa_part_id (sa_part_id);
  return sprintf ("sa_part_id(%d,%s,%s)",
                  sa_part_id,
                  (res[0] ? res[0]->obj_path() : "-"),
                  (res[1] ? res[1]->obj_path() : "-"));
}

REP.Story remove_story_app(int sa_part_id, void|REP.Story story,
                           bool|void ignore_no_current_pgv)
//! Removes a story appearance, moving the story to the publication
//! stack if there are no appearances left.
//!
//! @param sa_part_id
//! The story_app_parts.sa_part_id identifier of the appearance
//! record.
//!
//! @param story
//! The story of the appearance. This is optional and may be given to
//! save a database query.
//!
//! @returns
//! Returns the story object for the appearance, i.e. @[story] if it
//! was given.
//!
//! @seealso
//! @[remove_story_app_from_pgv]
{
  ASSERT_IF_DEBUG(sa_part_id);

  if (!story) {
    array(int) story_ids =
      story_apps_table()->select1 ("story_id",
                                   "story_app_parts.sa_part_id = " + sa_part_id,
                                   "JOIN story_app_parts USING (story_app_id)");

    if (!sizeof (story_ids)) {
      werror ("REP.DB.remove_story_app: sa_part_id %d not in DB, already "
              "removed?\n", sa_part_id);
      return UNDEFINED;
    }
    if (!(story = story_by_id (story_ids[0], REP.LOG_ERROR))) {
      return UNDEFINED;
    }
  }

  mapping(int:mapping(string:mixed)) sa_parts = story->story_app_parts();

  if (!sa_parts[sa_part_id]) {
    werror ("REP.DB.remove_story_app: sa_part_id %d not in story %O, app "
            "already removed?\n", sa_part_id, story);
    return UNDEFINED;
  }

  int story_app_id = sa_parts[sa_part_id]->story_app_id;
  int sa_part_count;
  foreach(sa_parts;; mapping(string:mixed) sa_part) {
    sa_part_count += (sa_part->story_app_id == story_app_id);
  }
  ASSERT_IF_DEBUG (story_app_id);

  REP.Edition edition = story->appearance_ed (sa_part_id);
  REP.PGVersion pgv = story->appearance_pg (sa_part_id, 0,
                                            ignore_no_current_pgv);

  story_app_parts_table()->cached_remove (sa_part_id);

  Sql.Sql db = REP.get_db();

  if (pgv)
    pgv->invalidate_appearances(1);

  if (sa_part_count == 1) {
    // Had only one appearance in this issue before,
    // so there are no remaining ones now.
    // FIXME: Race-condition!
    db->query("DELETE FROM story_apps "
              "      WHERE story_app_id = %d",
              story_app_id);

    array tmp = db->query("  SELECT COUNT(story_app_id) AS remaining "
                          "    FROM story_apps "
                          "   WHERE story_id = %d "
                          "GROUP BY story_id", story->id());
    int remaining = sizeof(tmp) && (int)(tmp[0]->remaining);

    if (!remaining) {
      // No appearances left, move to publication stack, indicated by
      // edition_id == NULL.
      db->query("INSERT INTO story_apps (story_id, edition_id) "
                "     VALUES (%d, NULL)", story->id());
      story->publication()->invalidate_story_appearances();
    }
  }

  story->invalidate_appearances();
  edition->invalidate_story_appearances();
  REP.get_print_module()->notify_story_apps_changed (story, pgv || edition);
  return story;
}

void remove_story_app_from_pgv(REP.Story story,
                               REP.PGVersion pgv)
//! Removes the story appearance between @[story] and @[pgv], moving
//! the story to the publication stack if there are no appearances
//! left. Does nothing if there is no such appearance.
{
  mapping(int:int) sa_parts =
    map (story->story_app_parts(), predef::`->, "page_group_id");
  if (int sa_part_id = search (sa_parts, pgv->get ("page_group_id")))
    remove_story_app (sa_part_id, story);
}

#if 0
REP.PGVersion.StorySlot story_slot_by_id (string story_slot_id,
                                          void|int only_existing,
                                          void|REP.OnError on_invalid_id)
{
  if (sscanf (story_slot_id, "%d:%*s", int pgv_id) == 2) {
    if (REP.PGVersion pgv = pgv_by_id (pgv_id, on_invalid_id)) {
      return pgv->story_slot_by_id (story_slot_id, only_existing,
                                    on_invalid_id);
    }
  }

  return REP.raise_err (on_invalid_id, "Invalid story slot identifier %s.\n",
                        story_slot_id);
}

REP.PGVersion.StorySlot story_slot_by_sa_part_id (int sa_part_id,
                                                  void|REP.OnError on_not_found)
{
  string slot_id = "";
  if (mapping(string:mixed) sa_part_rec =
      story_app_parts_table()->cached_get (sa_part_id))
    slot_id = sa_part_rec->story_slot;
  return story_slot_by_id (slot_id, 1, on_not_found);
}
#endif

protected REP.SIVersion low_make_infant_siv(REP.SIVersion src,
                                            void|REP.Story story)
//! Create an infant @[REP.SIVersion] object which can be modified
//! before it's committed. The object is not inserted into the tables
//! by this method.
//!
//! @param src
//! The source @[REP.SIVersion], which will be used as a base for the
//! new object. This may be 0 to indicate that no ancestor should be
//! set for the object.
//!
//! @returns
//! The newly created object in an uncommitted state.
{
  REP.SIVersion infant = story_item_versions_table()->create_infant();
  infant->parent_version = src || -1;

  if (src) {
    infant->si_volatile = src->si_volatile;
    infant->verbatim_copy(src, (["placed_page_group_id": 1, "state": 1]));
    mapping(string:mixed) fields = ([
      "date_created" : time(),
      "story_item_version_uuid" : Standards.UUID.make_version4()->str(),
      "user_id": REP.get_session()->user_id(),
    ]);
    infant->set_fields(fields);
  } else {
    // Fix si_volatile for the infant. Cannot (and need not) put it in
    // the global print_db->si_volatiles until it gets a story_item_id
    // (handled in SIVersion.low_set_raw_unver).
    infant->si_volatile = ([]);

    // Ensure set_fields always is called so various fields get their
    // default values.
    infant->set_unver_fields (([ "story_id": story->id() ]));
    infant->set_fields(([]));
  }

  return infant;
}

REP.SIVersion create_story_item (REP.Story story,
                                 REP.ComponentDef def,
                                 mapping(string:mixed) unver_fields,
                                 void|mapping(string:mixed) ver_fields,
                                 void|REP.OnError on_value_error)
//! Creates a new story item from a component definition, including
//! the first version of it.
//!
//! The story item is not automatically made current. Use
//! @[make_siv_current] for that.
//!
//! @param story
//! Containing story for the item.
//!
//! @param unver_fields
//! Data for unversioned portions of the story item.
//!
//! @param ver_fields
//! Data for the versioned portions of the story item. This parameter
//! will be used when initializing the first version of the story
//! item.
//!
//! @returns
//! The newly created @[REP.SIVersion] object which can be modified
//! until the caller calls @[REP.DB.make_siv_current] on it.
{
  REP.SIVersion siv = low_make_infant_siv(0, story);
  ASSERT_IF_DEBUG(def->id());

  siv->internal_set_data_fields_initials (def);

  if (!ver_fields) ver_fields = ([]);
  ver_fields->component_def_id = def->id();
  if (!siv->set_fields(ver_fields, on_value_error)) {
    return 0;
  }

  if (!siv->set_unver_fields(unver_fields, on_value_error)) {
    return 0;
  }

  siv = story_item_versions_table()->low_object_create(siv);
  story->invalidate_story_complete_cache();
  story->zap_items_cache(false, true);
  return siv;
}

REP.SIVersion copy_story_item (REP.SIVersion src, REP.Story dst,
                               void|int(0..1) prefer_placeholders,
                               void|mapping(string:int) map_id_output)
//! Creates a new story item in @[dst], copying all data fields in
//! @[src]. The copy will normally be added in the Extra area. However,
//! if copying takes place within the same story, or if the destination
//! story has an identical area, the new item will be added at
//! the end of that component area. By setting @[prefer_placeholders] we
//! will search for placeholders in the destination area and use them
//! instead.
//!
//! The story item is not automatically made current. Use
//! @[make_siv_current] for that.
//!
//! @returns
//! The newly created @[REP.SIVersion] object which can be modified
//! until the caller calls @[REP.DB.make_siv_current] on it.
{
  REP.SIVersion siv = low_make_infant_siv (0, src->story());
  ASSERT_IF_DEBUG (src->id());

  siv->internal_set_data_fields_initials (src->component_def());

  mapping(string:mixed) unver_fields =
    siv->copy_unver_fields & src->get_rec_unver();
  unver_fields->story = dst;

  //  Is the definition valid in the destination?
  REP.ComponentAreaDef ca_def = src->component_area_def();
  if (src->story() == dst) {
    //  Copy takes place within same story so ca_def exist unless we are
    //  targeting the Extra area so nothing needs to be done here.
  } else if (ca_def) {
    //  See if definitions are compatible
    REP.StoryTemplateDef dst_tmpl_def = dst->story_tmpl_def();
    if (has_value (dst_tmpl_def->component_area_defs(), ca_def)) {
      // The source item's ca_def exists in the destination story's
      // StoryTemplateDef - let's use it.
    } else {
      //  Check if both story templates are derived from same master since
      //  that ensure compatible areas as well, though we'll have to map
      //  manually.
      REP.StoryTemplateDef src_tmpl_def = ca_def->story_tmpl_def();
      REP.StoryTemplateDef src_root_def =
        src_tmpl_def->get_parent(REP.RETURN_ZERO) || src_tmpl_def;
      REP.StoryTemplateDef dst_root_def =
        dst_tmpl_def->get_parent(REP.RETURN_ZERO) || dst_tmpl_def;
      if (src_root_def == dst_root_def) {
        //  Look up with same name
        string ca_def_name = ca_def->name();
        ca_def =
          dst_tmpl_def->comp_area_def_by_name(ca_def_name, REP.RETURN_ZERO);
      } else {
        //  Failed to find match
        ca_def = 0;
      }
    }
  }

  //  New item is normally placed at end of area unless we are instructed
  //  to scan for placeholders.
  REP.SIVersion delete_placeholder_siv;
  if (ca_def) {
    array(REP.SIVersion) ca_sivs = dst->latest_items_by_area(ca_def);
    int ca_insert_pos = sizeof(ca_sivs) + 1;

    // If the caller allows reuse of empty/initial items we'll look
    // for those and reuse the placeholder's order value. The placeholder
    // itself will be deleted in the end since we will otherwise renumber
    // the items too early.
    if (prefer_placeholders) {
      for (int i = 0; i < sizeof(ca_sivs); i++) {
        if (ca_sivs[i]->all_fields_empty_or_initial()) {
          //  Remember position and then delete placeholder
          delete_placeholder_siv = ca_sivs[i];
          ca_insert_pos = delete_placeholder_siv->get_unver("comp_area_order");
          break;
        }
      }
    }

    unver_fields +=
      ([ "component_area_def_id" : ca_def->id(),
         "comp_area_order"       : ca_insert_pos ]);
  } else {
    unver_fields +=
      ([ "component_area_def_id": 0,
         "comp_area_order"      : sizeof (dst->latest_items_by_area (0)) + 1 ]);
  }

  //  Since the origin field may carry a reference to a feed item or an
  //  external writer we clear those in the copy. Also reset any imported
  //  date.
  unver_fields += ([ "origin": UNDEFINED, "date_imported": UNDEFINED ]);

  siv->set_unver_fields (unver_fields);

  // Note: date_created and user_id are not among the copied fields,
  // so the current time and user is kept in the copy.
  siv->set_fields (siv->copy_fields & src->get_rec());

  // Must commit the object before we can set external values in it.
  siv = story_item_versions_table()->low_object_create (siv);
  if (map_id_output) {
    map_id_output["si:" + src->get_unver("story_item_id")] =
      siv->get_unver("story_item_id");
  }

  REP.DBObject.DisableAutoCommit dac = siv->DisableAutoCommit();
  foreach (src->data_fields (1), REP.DataField df) {
    //  Copy field but clear attribute for deconstruct which is tied to the
    //  original's placement.
    REP.DataField df_copy = siv->copy_data_field (df);
    df_copy->set_attr("deconstruct", UNDEFINED);
  }

  dst->zap_items_cache(false, true);
  destruct (dac);

  //  Delete old placeholder if needed and force iten renumbering
  if (delete_placeholder_siv)
    delete_placeholder_siv->delete_story_item();

  dst->invalidate_story_complete_cache();
  return siv;
}

REP.SIVersion
create_story_item_from_feed_item (REP.Story story,
                                  void|REP.SIVersion old_siv,
                                  NewsFeed.Item item,
                                  mapping (string:mixed) unver_fields,
                                  void|mapping(string:mixed) ver_fields,
                                  void|REP.OnError on_value_error)
{
  int date_imported = item->get_date_imported();
  unver_fields += ([ "date_imported": date_imported ]);

  REP.SIVersion siv;
  if (old_siv) {
    //  Place data in existing siv
    siv = add_siv(old_siv, ver_fields, on_value_error);
    if (siv)
      siv->set_unver_fields(unver_fields);
  } else {
    //  Create new siv. We'll need to find a suitable component definition
    //  for the item type, either in the current publication configuration
    //  or in a global one. We prefer the basic components "ep_text" and
    //  "ep_image" but they may be unavailable due to overriding, in which
    //  case we compare compponent classes instead.
    //
    //  If the caller already provides a component area definition in the
    //  unversioned field mapping we take that as best choice.
    REP.ComponentDef use_comp_def;
    if (REP.ComponentAreaDef use_ca_def = unver_fields["component_area_def"]) {
      use_comp_def = use_ca_def->component_def();
    } else {
      string type = item->get_type();
      ASSERT_IF_DEBUG(type == "text" || type == "photo");
      string want_name  = (type == "text") ? "ep_text"  : "ep_image";
      string want_class = (type == "text") ? "rep.text" : "rep.image";

      REP.ComponentDef best_comp_def, good_comp_def, ok_comp_def;
      foreach (component_defs_for_publication(story->publication()),
               REP.ComponentDef comp_def) {
        if (comp_def->get_name() == want_name) {
          //  Exact match so stop here
          best_comp_def = comp_def;
          break;
        } else {
          //  Check for exact class or class prefix. A direct match is
          //  preferred to a prefix match. In either case we keep looking
          //  even if we find something.
          string cls = comp_def->get_class();
          if (cls == want_class) {
            if (!good_comp_def)
              good_comp_def = comp_def;
          } else if (has_prefix(cls, want_class + ".")) {
            if (!ok_comp_def)
              ok_comp_def = comp_def;
          }
        }
      }
      use_comp_def = best_comp_def || good_comp_def || ok_comp_def;
      if (!use_comp_def)
        return REP.raise_err(on_value_error,
                             "Could not find suitable component definition "
                             "for feed item of type %s (component class %s).\n",
                             type, want_class);
    }

    siv = create_story_item (story, use_comp_def, unver_fields,
                             ver_fields, on_value_error);
  }
  if (!siv)
    return REP.raise_err(on_value_error, "Could not add new item.\n");

  //  Make a copy since we'll operate destructively on values below
  mapping(string:mixed) md = item->get_metadata() + ([ ]);

  //  Peek at item fields to see if any values should be grabbed from a
  //  rich-text body style.
  string body_richtext = item->get_richtext();
  mapping(string:array(Parser.XML.Tree.Node)) grabbed_nodes = ([ ]);
  if (body_richtext) {
    //  Build node tree before we lose styles in the conversion step
    Parser.XML.Tree.Node root =
      Parser.XML.Tree.parse_input("<root>" + body_richtext + "</root>");
    foreach (siv->data_fields (1), REP.DataField field) {
      REP.FieldDef fd = field->field_def();
      if (array(string) from_style_globs = fd->get_initial_from_body_style()) {
        //  This field wants one or more body text paragraphs. We'll scan
        //  through the body and move any nodes from the flow into the
        //  special list.
        array(Parser.XML.Tree.Node) grab_nodes = ({ });
        root->walk_preorder(lambda(Parser.XML.Tree.Node n) {
                              if (n->get_tag_name() == "div") {
                                if (string cls = n->get_attributes()["class"]) {
                                  if (!has_prefix(cls, "p_"))
                                    return;
                                  cls = lower_case(cls[2..]);
                                  foreach (from_style_globs, string glb)
                                    if (glob(glb, cls)) {
                                      //  Grab this node
                                      grab_nodes += ({ n });
                                      break;
                                    }
                                }
                              }
                            });
        if (sizeof(grab_nodes)) {
          //  Unlink from body structure
          foreach (grab_nodes, Parser.XML.Tree.Node n)
            n->get_parent()->remove_child(n);

          string fname = field->name();
          if (grabbed_nodes[fname])
            grabbed_nodes[fname] += grab_nodes;
          else
            grabbed_nodes[fname] = grab_nodes;
        }
      }
    }

    //  If we've mutated the body tree we need to stringify the body again
    //  for the conversion below.
    if (sizeof(grabbed_nodes))
      body_richtext = root[0]->get_children()->html_of_node() * "";

    root->zap_tree();
  }

  siv->set_unver_fields (([ "origin" : (REP.Types.feed_origin_type +
                                        item->get_handle()) ]));

  //  Populate item fields in definition order to get some
  //  predictability across multiple stories. The exception is image
  //  fields, which we want to process first to make sure any iptciim
  //  data provider has something to work with for related text
  //  fields.
  //
  //  NOTE: Updating a field that acts as a data provider for other fields
  //  will trigger an async update of its consumers through internal
  //  callbacks (e.g. FieldDef::data_field_changed()). Those updates will
  //  race against our own initialization below.
  multiset(string) source_used = (< >);
  array(REP.DataField) data_fields = siv->data_fields();

  array(REP.DataField) image_fields =
    filter (data_fields,
            lambda (REP.DataField df)
            {
              return df->field_def()->get ("type") == "image";
            });
  array(string) image_field_names = image_fields->name();

  data_fields = image_fields + (data_fields - image_fields);
  array(string) image_cts = REP.LDE.field_to_mime_type["image"];

  multiset(string) data_field_names = (multiset) data_fields->name();
  foreach (data_fields, REP.DataField field) {
    mixed value;
    string source_field;

    switch (field->name()) {
    case "headline":
    case "rubrik":
    case "tittel":
      value = item->get_title();
      source_field = "title";
      break;

    case "filename":
      value = basename(item->get_raw_filepath() || item->get_md_filepath());
      source_field = "filepath";
      break;

    case "blurb":
    case "subheadline":
    case "ingress":
      //  Prefer subtitle but caption will do unless a real caption field
      //  is present in the definition.
      value = item->get_sub_title();
      source_field = "subtitle";
      if (value && value != "") break;
      if (!data_field_names["caption"] &&
          !data_field_names["bildtext"] &&
          !data_field_names["bildetext"]) {
        value = m_delete(md, "caption-abstract");
        source_field = "caption";
      }
      break;

    case "body":
    case "bodytext":
    case "brodtext":
    case "brodtekst":
      //  Attempt rich-text if item provides it and field type matches
      if (body_richtext) {
        REP.FieldDef fd = field->field_def();
        if (fd->get_type() == "rich-text") {
          string html =
            REP.LDE.convert_value_to_type(body_richtext, "rich-text", fd);
          field->set_data(html);
          source_used["text"] = 1;
          continue;
        }
      }

      //  Fallback to plaintext
      value = item->get_text();
      source_field = "text";
      break;

    case "byline":
      //  Prefer byline to credit, but both will do
      value = m_delete(md, "by-line");
      source_field = "md:by-line";
      if (value && value != "") break;
      value = m_delete(md, "credit");
      source_field = "md:credit";
      break;

    case "notes":
      value = item->get_notes();
      source_field = "notes";
      break;

    case "credit":
      //  Prefer credit to byline, but both will do
      value = m_delete(md, "credit");
      source_field = "md:credit";
      if (value && value != "") break;
      value = m_delete(md, "by-line");
      source_field = "md:by-line";
      break;

    case "caption":
    case "bildtext":
    case "bildetext":
      //  NB: See "blurb" etc above for another mapping of caption with
      //  lower priority.
      value = m_delete(md, "caption-abstract");
      source_field = "caption";
      break;

    default:
      //  We primarily want a feed image to go into the earmarked fields
      //  listed in the case statements below. However, if none of those
      //  are present we should pick the first image-capable field available.
      if (!has_value(image_fields, field) ||
          sizeof(image_field_names & ({ "image", "bild", "bilde" }) ))
        break;
      //  NOTE: Fallthrough
    case "image":
    case "bild":
    case "bilde":
      //  Only accept files of supported image types
      string ct = item->get_raw_mimetype();
      if (has_value(image_cts, ct) && !source_used["raw"]) {
        Stdio.Stream src_fd = item->get_raw_file_stream();
        field->set_in_file (src_fd, UNDEFINED, ct);
        field->set_attr ("orig_filename", item->get_basename());
        source_used["raw"] = 1;
      }
      continue;
    }

    if (value == "") {
      value = UNDEFINED;
    }

    if (!value) {
      source_used[source_field] = 1;

      // Attempt to get content from the data provider (if any) instead.
      if (REP.DataProvider.Provider prov = field->get_data_provider()) {
        if (prov->name == "feed-import" ||
            prov->name == "feed-import-xml" ||
            prov->name == "iptciim") {
          //  No point in checking is_empty_or_initial() directly after
          //  update call since data provider execution is asynchronous.
          //  We've already given priority to feed item data so we skip
          //  to next field directly.
          field->update_from_data_provider();
          continue;
        }
      }
    }

    //  If a rich-text value was extracted from the body field we'll let
    //  that take precedence of whatever we prepared above.
    if (array(Parser.XML.Tree.Node) body_nodes = grabbed_nodes[field->name()]) {
      string xml = body_nodes->html_of_node() * "";
      value = REP.LDE.convert_value_to_type(xml, "rich-text",
                                            field->field_def());
      field->set_data(value);

      //  No need to track that these grabbed nodes (which were already
      //  unlinked from the feed body) have been used since their grabbing
      //  was already based on the fact that they would be used.
    } else if (value) {
      if (arrayp (value))
        value = value * " ";

      //  Normalize to field definition (e.g. converting newlines). We give
      //  the old data the type "plain-text-multiline" which is our best
      //  guess for text-based fields at the moment.
      value = REP.LDE.convert_value_to_type(value, "plain-text-multiline",
                                            field->field_def());

      field->set_data (value);
      source_used[source_field] = 1;
    }
  }

  //  Any feed data that hasn't been used at this point should be added
  //  as extra fields so the data isn't lost.
  mapping(string:string) source_remaining =
    ([ "title"               : item->get_title(),
       "subtitle"            : item->get_sub_title(),
       "text"                : body_richtext || item->get_text(),
       "caption"             : md["caption-abstract"],
       "notes"               : item->get_notes(),
       //"raw"               : 0,
       "md:by-line"          : md["by-line"],
       "md:by-line-title"    : md["by-line-title"],
       "md:credit"           : md["credit"],
       "md:source"           : md["source"],
       "md:city"             : md["city"],
       "md:datetime"         : ( ({ stringp(md["date"]) && md["date"],
                                    stringp(md["time"]) && md["time"] }) -
                                 ({ 0 }) ) * ", "
    ]) - source_used;
  foreach (source_remaining; string key; string|array val) {
    //  Some items (e.g. caption-abstract) seem to be arrays at times so
    //  we convert them to plain strings.
    if (arrayp(val)) {
      val = val * " ";
      source_remaining[key] = val;
    }

    if (!stringp(val) || val == "")
      m_delete(source_remaining, key);
  }

  //  Combine several metadata fields into one
  array(string) md_misc_fields =
    ({ "md:by-line", "md:by-line-title", "md:credit", "md:source",
       "md:city", "md:datetime" });
  array(string) md_misc = map(md_misc_fields, source_remaining) - ({ 0 });
  source_remaining -= md_misc_fields;

  void add_extra_field(string prompt, string type, string data) {
    //  An orphaned field must have a corresponding field definition, and
    //  that in turn needs a parent component definition. We'll add these
    //  on-demand and reference them from the data field in the siv even
    //  though the siv's own component definition won't see them.
    mapping label = ([ "en" : prompt ]);
    if (REP.DataField orphan_field = siv->add_orphan_field(label, type)) {
      orphan_field->set_data (data);
    }
  };

  foreach (source_remaining; string field; string val) {
    string field_type = "plain-text";
    if (field == "text" && body_richtext) {
      field_type = "rich-text";
    } else if (sizeof(val / "\n") > 1) {
      field_type = "plain-text-multiline";
    }
    add_extra_field("Feed: " + String.capitalize(field), field_type, val);
  }
  if (sizeof(md_misc)) {
    add_extra_field("Feed: Metadata", "plain-text-multiline", md_misc * "\n");
  }

  return siv;
}

REP.SIVersion add_siv (REP.SIVersion old_siv,
                       void|mapping(string:mixed) changed_fields,
                       void|REP.OnError on_value_error)
//! Adds a new @[REP.SIVersion] to the history axis of a story item.
//!
//! The new story item version is not automatically made current. Use
//! @[make_siv_current] for that. @[set_in_current_siv] is often more
//! convenient.
//!
//! @param old_siv
//! The old @[REP.SIVersion] object that precedes the one which is created.
//!
//! @param changed_fields
//! Changes that are made to the set of data fields in the
//! @[REP.SIVersion] object. Only versioned fields may be set this
//! way.
//!
//! @returns
//! The newly created @[REP.SIVersion] object if successful.
{
  ASSERT_IF_DEBUG(old_siv);
  REP.SIVersion infant = low_make_infant_siv(old_siv);
  if (changed_fields && !infant->set_fields(changed_fields, on_value_error))
    return 0;

  REP.Story story = old_siv->story();
  REP.SIVersion new_siv = story_item_versions_table()->low_object_create(infant);
  story->zap_items_cache(false, true);
  return new_siv;
}

REP.SIVersion add_latest_siv (REP.SIVersion current_siv,
                              string siv_lock,
                              void|mapping(string:mixed) changed_fields,
                              void|REP.OnError on_lock_error,
                              void|REP.OnError on_value_error)
//! Adds a new latest @[REP.SIVersion] (that is not current) unless
//! one exists already.
//!
//! @param current_siv
//!   The current @[REP.SIVersion].
//!
//! @param siv_lock
//!   The lock for this story item.
//!
//! @param changed_fields
//!   See @[add_siv].
//!
//! @param on_lock_error
//!   What to do if @[siv_lock] is not a valid lock for this story item.
//!
//! @param on_value_error
//!   See @[add_siv].
{
  if (check_siv_lock (current_siv, siv_lock)) {
    ASSERT_IF_DEBUG (current_siv == current_siv->current_siv());
    REP.SIVersion latest_siv = current_siv->latest_siv();
    if (latest_siv == current_siv) {
      return add_siv (current_siv, changed_fields, on_value_error);
    } else {
      if (changed_fields)
        latest_siv->set_fields (changed_fields);
      return latest_siv;
    }
  }

  return REP.raise_err (on_lock_error,
                        "%O not locked with key %O\n",
                        current_siv,
                        siv_lock);
}

protected int(0..1)
unlocked_make_siv_current(REP.SIVersion new_siv,
                          void|REP.SIVersion old_siv,
                          void|REP.OnError on_stale_siv)
//! Sets a new @[REP.SIVersion] as the current one for a given story
//! item id, under the assumption that all tables involved have been
//! locked by the caller.
{
  ASSERT_IF_DEBUG (new_siv->get ("story_item_id"));
  ASSERT_IF_DEBUG (new_siv->get ("story_item_version_uuid"));

  ASSERT_IF_DEBUG(!old_siv || (old_siv->get("story_item_id") ==
                               new_siv->get("story_item_id")));
  ASSERT_IF_DEBUG(old_siv != new_siv);
  ASSERT_IF_DEBUG(!old_siv || old_siv->id() /* %O */ < new_siv->id() /* %O */,
                  old_siv, new_siv);

  REP.SIVersion cur_siv = new_siv->current_siv();
  if (!zero_type (old_siv) && old_siv != cur_siv)
    return REP.raise_err (on_stale_siv,
                          "Cannot make stale story item version "
                          "%O current (latest is %O but expected %O)\n",
                          new_siv, cur_siv, old_siv);

  if (cur_siv && new_siv->id() <= cur_siv->id())
    return 1;

  story_items_table()->
    cached_update((["story_item_id" : new_siv->get("story_item_id"),
                    "cur_siversion_id" : new_siv->id()]));

  // Insta-garb handled in SIVersion.ver_record_hook.

  story_item_versions_table()->
    schedule_record_notification ("new_version", new_siv, cur_siv);

  new_siv->parent_version = 0;

  new_siv->story()->zap_items_cache(true, true);

  return 1;
}

int(0..1) make_siv_current(REP.SIVersion new_siv,
                           void|REP.SIVersion old_siv,
                           void|REP.OnError on_stale_siv)
//! Sets the @[new_siv] as the current version for the story item.
//!
//! @param new_siv
//! The new @[REP.SIVersion] that should become the current one.
//!
//! @param old_siv
//! The expected current story item version, which may be zero if
//! @[new_siv] is the first version. Pass @[UNDEFINED] to not check
//! the current siv at all (which introduces a race unless the
//! story_item_versions table is locked for the duration).
//!
//! @param on_stale_siv
//! Determines how the method will behave in the case where @[old_siv]
//! doesn't match the current story item version.
//!
//! @returns
//! 1 on success.
//!
//! @note
//! If @[old_siv] isn't checked, and @[new_siv] already is current or
//! old, then nothing happens and 1 is returned.
{
  TableLock lock = TableLock((["story_items": WRITE_LOCK,
                               "story_item_versions": WRITE_LOCK,
                               "stories": READ_LOCK]));

  int ok = unlocked_make_siv_current(new_siv, old_siv, on_stale_siv);
  destruct(lock);
  return ok;
}

REP.SIVersion siv_by_id (int(1..) story_item_version_id,
                         void|REP.OnError on_rec_not_found)
{
  return story_item_versions_table()->object_get (story_item_version_id,
                                                  on_rec_not_found);
}

REP.SIVersion siv_by_uuid(string story_item_version_uuid,
                          void|REP.OnError on_rec_not_found)
{
  REP.ObjectTable tbl = story_item_versions_table();
  REP.SIVersion siv = siv_by_id (rec_id_by_uuid (tbl, story_item_version_uuid),
                                 REP.RETURN_ZERO);

  if (siv)
    return siv;

  return REP.raise_err(on_rec_not_found,
                       "Story item version with uuid %s not found.\n",
                       story_item_version_uuid);
}

REP.SIVersion current_siv_by_si_id(int story_item_id,
                                   void|REP.OnError on_rec_not_found)
//! Returns the current story item version, i.e. the latest
//! @[REP.SIVersion] that has been made current by
//! @[make_siv_current].
//!
//! @param story_item_id
//! The story item id.
//!
//! @returns
//! The current @[REP.SIVersion] object for the given story item id or
//! 0 if no current object is found.
{
  mapping(string:mixed) si =
    story_items_table()->cached_get(story_item_id, on_rec_not_found);
  if (!si)
    return 0;

  int|Val.Null last_siv_id = si->cur_siversion_id;

  return !zero_type (last_siv_id) &&
    story_item_versions_table()->object_get(last_siv_id, on_rec_not_found);
}

REP.SIVersion current_siv_by_si_uuid (string story_item_uuid,
                                      void|REP.OnError on_rec_not_found)
//! Like @[current_siv_by_si_id], but takes a story item uuid as
//! argument instead.
{
  return current_siv_by_si_id (rec_id_by_uuid (story_items_table(),
                                               story_item_uuid),
                               on_rec_not_found);
}

REP.SIVersion latest_siv_by_si_id (int story_item_id,
                                   void|REP.OnError on_rec_not_found)
//! Returns the latest story item version, which might be later than
//! the one returned by @[current_siv_by_si_id] if there is one that
//! hasn't yet been made current.
{
  array(REP.SIVersion) items = sivs_by_si_id (story_item_id, 1);

  if (!sizeof (items))
    return REP.raise_err (on_rec_not_found, "story_item_id %d not found.\n",
                          story_item_id);

  return items[0];
}

//! Returns an array of SIVersion objects for the given StoryItem id.
//!
//! @seealso
//!   @[sivs_by_si_uuid()]
array(REP.SIVersion) sivs_by_si_id(int si_id,
                                   void|int(1..) limit,
                                   void|int include_deleted)
{
  REP.SIVersion siv = current_siv_by_si_id (si_id, REP.RETURN_ZERO);

  if (!siv) {
    // No current pgv for the specified story item. We'll fetch one
    // from the DB (this is a pretty rare case).
    REP.ObjectTable siv_tbl = story_item_versions_table();
    array(REP.SIVersion) sivs =
      siv_tbl->object_select ("story_item_id = " + si_id,
                              0,
                              "LIMIT 1");
    if (sizeof (sivs))
      siv = sivs[0];
    else
      return ({});
  }

  return siv->all_versions (limit, include_deleted);
}

//! Returns an array of SIVersion objects for the given StoryItem uuid.
//!
//! @seealso
//!   @[sivs_by_si_id()]
array(REP.SIVersion) sivs_by_si_uuid(string si_uuid,
                                   void|int(1..) limit,
                                   void|int include_deleted) {
  REP.CachedTable si_tbl = story_items_table();
  array(mapping) res = si_tbl->cached_select("story_item_uuid = '" +
                                             si_tbl->quote(si_uuid) + "'");
  if (!sizeof(res)) return ({});
  return sivs_by_si_id(res[0]->story_item_id, limit, include_deleted);
}

array(REP.SIVersion) current_sivs_by_origin (REP.Types.OriginType type,
                                             string origin,
                                             void|int include_deleted)
{
  REP.ObjectTable si_version_tbl = story_item_versions_table();
  REP.CachedTable si_tbl = story_items_table();
  return si_version_tbl
    ->object_get_multi (si_tbl
                        ->select1 ("cur_siversion_id",
                                   sprintf("`origin` = '%s'" +
                                           (include_deleted ?
                                            "" :
                                            " AND delete_at = 0"),
                                           si_tbl->quote (type + origin))));

}

// Note: No need for this to be persistent, since we embed
//       the time of the lock operation in the key, and
//       restarting Roxen typically takes more than one second.
protected int lock_counter;

//! Returns an array of currently locked @[REP.SIVersion] for a
//! specified user, if any, otherwise the empty array.
array(REP.SIVersion) locked_sivs_by_user (string user)
{
  REP.ObjectTable si_version_tbl = story_item_versions_table();
  REP.CachedTable si_tbl = story_items_table();
  return si_version_tbl
    ->object_get_multi (si_tbl
                        ->select1 ("cur_siversion_id",
                                   sprintf("`lock` LIKE '%s'",
                                           si_tbl->quote (user) + ":%")));
}

void unlock_all_sivs_for_user (string user)
{
  foreach (locked_sivs_by_user (user), REP.SIVersion siv) {
    if (string key = get_siv_lock (siv)) {
      unlock_siv (siv, key);
    }
  }
}

string user_from_siv_lock(string key)
{
  //  Key is formatted as "user:timestamp:counter"
  return key && (key / ":")[0];
}

protected string generate_lock_key (string locker)
{
  return sprintf("%s:%d:%d", locker, time(1), lock_counter++);
}

//! Attempt to lock a SIVersion.
//!
//! @param siv
//!   The @[REP.SIVersion] to lock.
//!
//! @param locker
//!   A string identifying the locker, typically a username. Iff zero,
//!   the locker will be resolved from the RequestID of the current
//!   session.
//!
//! @returns
//!   Returns the siv lock on success, and @expr{0@} (zero) on failure.
string try_lock_siv(REP.SIVersion siv, void|string locker)
{
  if (!locker) {
    locker = REP.get_session()->user_handle();
    if (!locker) {
      error ("No locker string provided and no user handle for this "
             "session.\n");
    }
  }

  string key = generate_lock_key (locker);
  return story_items_table()->
    cached_test_and_update(siv->get("story_item_id"), "lock", key, Val.null) &&
    key;
}

//! Attempt to re-lock an already locked SIVersion.
//!
//! @param siv
//!   The @[REP.SIVersion] to lock.
//!
//! @param current_key
//!   What the caller thinks is the current key. If this doesn't match
//!   the actual current key, the relock will fail.
//!
//! @param locker
//!   A string identifying the locker, typically a username. Iff zero,
//!   the locker will be resolved from the RequestID of the current
//!   session.
//!
//! @returns
//!   Returns the siv lock on success, and @expr{0@} (zero) on failure.
string relock_siv (REP.SIVersion siv, string current_key, void|string locker)
{
  if (!locker) {
    locker = REP.get_session()->user_handle();
    if (!locker) {
      error ("No locker string provided and no user handle for this "
             "session.\n");
    }
  }

  string new_key = generate_lock_key (locker);
  return story_items_table()->
    cached_test_and_update(siv->get("story_item_id"), "lock",
                           new_key, current_key) && new_key;
}

//! Get the current lock from a SIVersion (if any).
//!
//! @returns
//!   Returns the siv lock if the SIVersion is locked,
//!   and @expr{0@} (zero) if it isn't locked.
string get_siv_lock(REP.SIVersion siv)
{
  string|Val.Null res = siv->get_unver("lock");
  return !objectp(res) && res;
}

//! Check if the @[key] is valid for the SIVersion.
//!
//! @returns
//!   Returns @expr{1@} if the key is valid and @expr{0@} (zero) on failure.
int(0..1) check_siv_lock(REP.SIVersion siv, string key)
{
  return siv->get_unver("lock") == key;
}

//! Check if @[siv] is locked by the user @[user].
//!
//! @param user
//!   The user handle to check against. Iff zero, the user handle will
//!   be resolved from the RequestID of the current session.
int(0..1) check_siv_lock_user (REP.SIVersion siv, void|string user)
{
  if (!user) {
    user = REP.get_session()->user_handle();
    if (!user) {
      error ("No locker string provided and no user handle for this "
             "session.\n");
    }
  }

  return user_from_siv_lock (get_siv_lock (siv)) == user;
}

//! Unlock a SIVersion given a key.
//!
//! @returns
//!   Returns @expr{1@} on success and @expr{0@} (zero) on failure.
int unlock_siv(REP.SIVersion siv, string key)
{
  return story_items_table()->
    cached_test_and_update(siv->get("story_item_id"), "lock", Val.null, key);
}

//! Forcefully unlock a SIVersion without knowing the key.
//!
//! @returns
//!   Returns name of last user holding the lock on success and
//!   @expr{0@} (zero) if the item wasn't locked or if unlocking failed.
string|void force_unlock_siv(REP.SIVersion siv)
{
  if (string key = get_siv_lock(siv))
    if (unlock_siv(siv, key))
      return user_from_siv_lock(key);
  return 0;
}

//! Switch component area definition for an SIVersion.
//! Takes care of component definition conversion, if needed.
//!
//! @param siv
//!   The SIVersion to switch area definition for.
//!
//! @param ca_def
//!   The component area definition to switch to.
//!
//! @param position
//!   The new position within the component area.
//!
//! @returns
//!   The new SIVersion if we needed to perform component definition
//!   conversion or 0 if no conversion was needed.
REP.SIVersion set_siv_component_area_def (REP.SIVersion siv,
                                          REP.ComponentAreaDef ca_def,
                                          void|int position)
{
  mapping(string:mixed) rec =
    ([ "component_area_def_id" : ca_def && ca_def->id() ]);

  if (position)
    rec->comp_area_order = position;
  else
    rec->comp_area_order =
      sizeof (siv->story()->latest_items_by_area (ca_def)) + 1;


  if (ca_def) {
    REP.ComponentDef ca_comp_def = ca_def->component_def();
    REP.ComponentDef ca_parent_def = ca_comp_def->get_parent() || ca_comp_def;
    REP.ComponentDef siv_comp_def = siv->component_def();
    REP.ComponentDef siv_parent_def =
      siv_comp_def->get_parent() || siv_comp_def;

    if (ca_comp_def != siv_comp_def &&
        !ca_comp_def->is_current()) {
      if (siv_parent_def == ca_parent_def) {
        if (siv_comp_def->is_current()) {
          ca_comp_def = siv_comp_def;
        } else if (array(REP.ComponentDef) derived_defs =
                   ca_parent_def->derived_defs()) {
          foreach (derived_defs, REP.ComponentDef def) {
            // Attempt to find a current def with the same name as the
            // older one used by our siv.
            if (def->get_name() == siv_comp_def->get_name())
              ca_comp_def = def;
          }
        }
      }
    }

    if (ca_comp_def != siv_comp_def) {
      // The component definition will change. We need to add a new
      // version to accommodate that.

      REP.SIVersion new_siv = REP.DB.add_siv(siv);
      new_siv->internal_switch_component_definition (ca_comp_def);

      // Make sure we switch atomically.
      TableLock lock = TableLock((["story_items": WRITE_LOCK,
                                   "story_item_versions": WRITE_LOCK,
                                   "stories": READ_LOCK ]));

      new_siv->low_set_raw_unver (rec);
      unlocked_make_siv_current (new_siv);
      destruct (lock);
      return new_siv;
    }
  }

  // No component definition change needed. This is simple, just set
  // the new ComponentAreaDef and position (if any).
  siv->low_set_raw_unver (rec);

  return 0;
}

//! Switch component definition for an SIVersion.
//! Takes care of component definition conversion.
//!
//! @param siv
//!   The SIVersion to switch component definition for.
//!
//! @param comp_def
//!   The component definition to switch to.
//!
//! @returns
//!   The new SIVersion that was created to accommodate the component
//!   definition switch.
REP.SIVersion set_siv_component_def (REP.SIVersion siv,
                                     REP.ComponentDef comp_def)
{
  REP.SIVersion new_siv = REP.DB.add_siv(siv);
  new_siv->internal_switch_component_definition (comp_def);
  make_siv_current (new_siv, siv);

  return new_siv;
}

array(int) add_data_field_placement (REP.DataField df, void|REP.PGVersion pgv)
//! Adds a data field placement to the placements table. Typically
//! called by the InDesign module when it detects that a
//! @[REP.DataField] was placed, either in a @[REP.PGVersion] or
//! previewed using a library snippet.
//!
//! @param df
//! The @[REP.DataField] that was placed. Note that the version is
//! significant, i.e. use the field from the correct @[REP.SIVersion]
//! for a story item.
//!
//! @param pgv
//! The @[REP.PGVersion] that the field was placed in. The version is
//! significant, and all @[REP.PGVersion]s sharing the same storage counter
//! will be updated. If this argument isn't provided or zero, it means
//! that the field was previewed using a library snippet.
//!
//! @returns
//! The data_field_placement_ids that were created/updated.
{
  SqlTools.SqlTable dfp_table = data_field_placements_table();

  REP.FieldDef fd = df->field_def();
  REP.SIVersion cur_siv = df->siv();
  mapping(REP.SIVersion:mapping(string:mixed)) recs_by_siv =
    ([ cur_siv : ([ "story_item_version_id" : cur_siv->id(),
                    "field_def_id"          : fd->id() ]) ]);

  //  If this data field uses external storage shared with a previous SIV
  //  we need to add a placement flag there as well to avoid later gc
  //  removal if that SIV is placed permamently in a layout document.
  if (pgv && !fd->data_sent_inline) {
    //  NOTE: We're assuming it is the "data" attribute of the given field
    //        that is placed.
    if (int old_siv_id = df->get_storage_id(0)) {
      if (old_siv_id != cur_siv->id()) {
        if (REP.SIVersion old_siv =
            REP.DB.siv_by_id(old_siv_id, REP.RETURN_ZERO)) {
          recs_by_siv[old_siv] = ([ "story_item_version_id" : old_siv_id,
                                    "field_def_id"          : fd->id() ]);
        }
      }
    }
  }

  array(int(0..0)|REP.PGVersion) loop_pgvs;
  if (pgv) {
    loop_pgvs = REP.DB.pgvs_by_storage_counter (pgv->get ("storage_counter"));
  } else {
    loop_pgvs = ({ 0 });
  }

  array(int) dfp_ids = ({});
  foreach (loop_pgvs, REP.PGVersion pgv) {
    foreach (recs_by_siv; REP.SIVersion siv; mapping(string:mixed) rec) {
      mapping(string:mixed) loop_rec = rec + ([]);
      //  If page_version_id isn't given it will be treated as NULL
      if (pgv) loop_rec["page_version_id"] = pgv->id();

      dfp_ids += ({ dfp_table->insert_or_update (loop_rec) });
      siv->update_df_placements_cache_rec (loop_rec);
    }
  }
  return dfp_ids;
}

void delete_data_field_preview_placement (REP.DataField|REP.SIVersion df_or_siv)
//! Deletes one or more data field placements. This is only relevant for
//! library preview placements since flags associated to @[REP.PGVersion]
//! objects are version-controlled and cannot change retroactively.
//!
//! @param df_or_siv
//! The @[REP.DataField] or @[REP.SIVersion] to delete placements
//! for. If a @[REP.SIVersion] is provided, all corresponding
//! @[REP.DataField] placements will be deleted.
{
  ASSERT_IF_DEBUG (df_or_siv);
  ASSERT_IF_DEBUG (df_or_siv->is_rep_siv || df_or_siv->is_rep_data_field);

  SqlTools.SqlTable dfp_table = data_field_placements_table();

  REP.SIVersion siv;
  REP.DataField df;

  if (df_or_siv->is_rep_data_field) {
    df = df_or_siv;
    siv = df->siv();
  } else {
    siv = df_or_siv;
  }

  mapping(string:mixed) conds = ([ "page_version_id" : 0 ]);

  if (siv) conds["story_item_version_id"] = siv->id();
  if (df) conds["field_def_id"] = df->field_def()->id();

  mapping(string:mixed) bindings = ([]);
  array(string) where = ({});
  foreach (conds; string index; mixed value) {
    //  We need NULL handling for the zero page_version_id value, but only
    //  here and not in the call to siv->remove_df_placements_cache_rec().
    bindings[":" + index] = value ? value : Val.null;
    where += ({ index + " = :" + index });
  }

  // Just a precaution to avoid zapping the entire table if
  // something's calling us without arguments...
  if (!sizeof (where)) return;

  array(int) update_siv_ids;
  if (!siv) {
    update_siv_ids = dfp_table->select1 ("DISTINCT story_item_version_id",
                                         ({ where * " AND ", bindings }));
  }

  dfp_table->delete (({ where * " AND ", bindings }));

  if (siv) {
    siv->remove_df_placements_cache_rec (conds);
  }

  if (update_siv_ids && sizeof (update_siv_ids)) {
    // Update the RAM state of any instantiated SIVersion objects. The
    // reason we want to avoid instantiating objects is not only for
    // performance, but also that instantiation may cause DB lookups
    // in tables we don't have table locks for.
    REP.ObjectTable siv_table = story_item_versions_table();
    foreach (update_siv_ids, int update_siv_id) {
      if (REP.SIVersion update_siv = siv_table->get_from_cache (update_siv_id))
        update_siv->remove_df_placements_cache_rec (conds);
    }
  }
}

void copy_data_field_placements (REP.PGVersion src_pgv, REP.PGVersion dst_pgv)
//! Copies all data field placements from @[src_pgv] to
//! @[dst_pgv]. Typically used when a @[REP.PGVersion] is copied with
//! its contents (layout file) intact, and no deconstruct will
//! occur. Note that copying isn't needed when the layout file of one
//! @[REP.PGVersion] is copied to the UNPROC_LAYOUT_FILE of another,
//! since the deconstruct will update placements accordingly.
{
  Sql.Sql db = REP.get_db();

  // Using a subquery would be much easier, but with table locks in
  // place we'd need a second table lock for the subquery, which seems
  // pretty complicated with the current TableLock implementation...
  array(mapping(string:mixed)) res =
    db->typed_query ("SELECT story_item_version_id, field_def_id "
                     "  FROM data_field_placements " +
                     " WHERE page_version_id = %d",
                     src_pgv->id());

  if (sizeof (res)) {
    int dst_pgv_id = dst_pgv->id();
    array(int) notify_sivs = ({});
    db->query ("INSERT IGNORE INTO data_field_placements "
               " (page_version_id, story_item_version_id, field_def_id) "
               " VALUES (" +
               (map (res, lambda (mapping(string:mixed) row)
                          {
                            int siv_id = row->story_item_version_id;
                            notify_sivs += ({ siv_id });
                            return sprintf ("'%d', '%d', '%d'",
                                            dst_pgv_id,
                                            siv_id,
                                            row->field_def_id);
                          }) * "),(") +
               ")");
    foreach (notify_sivs, int siv_id) {
      if (REP.SIVersion siv = REP.DB.siv_by_id (siv_id, REP.RETURN_ZERO))
        siv->update_df_placements_cache();
    }
  }
}

array(REP.DataField) data_field_placements_in_pgv(REP.PGVersion pgv,
                                                  void|REP.Story story)
{
  Sql.Sql db = REP.get_db();
  array(mapping(string:mixed)) rows =
    db->typed_query ("SELECT story_item_version_id, field_def_id "
                     "  FROM data_field_placements " +
                     " WHERE page_version_id = %d",
                     pgv->id());

  //  Resolve to SIVs and scope to given story if necessary
  array(REP.DataField) res = ({ });
  foreach (rows, mapping(string:mixed) row) {
    if (REP.SIVersion siv =
        REP.DB.siv_by_id(row->story_item_version_id, REP.RETURN_ZERO)) {
      if (!story || (siv->story() == story)) {
        if (REP.FieldDef fd =
            REP.DB.field_def_by_id(row->field_def_id, REP.RETURN_ZERO)) {
          //  Resolve to datafield within this SIV
          if (REP.DataField df =
              siv->data_field_by_name(fd->get_name(), REP.RETURN_ZERO))
            res += ({ df });
        }
      }
    }
  }

  return res;
}

// Looking up placements for a SIV: See SIVersion.data_field_placement_pgvs.


//
// Search stuffs here...
// This mapping should be configurable!
protected constant story_item_search_field_map = ([
  "headline" : "headline",
  "title"    : "title",
  "body"     : "body",
  "blurb"    : "blurb",
]);

string map_field_to_search_field(REP.DataField f) {
  string f_name = f->name();
  return story_item_search_field_map[f_name];
}

// FIXME: This function traverses mappings/arrays and substitutes
// Val.true, Val.false, Val.null by 1 or 0 to make
// encode_value_canonic (in SIVersion.commit) able to encode the
// structure. Do something better<tm>.
mixed decode_objects (mixed value)
{
  if (mappingp (value)) {
    mapping ret = ([]);
    foreach (value; mixed ind; mixed val) {
      ret[ind] = decode_objects (val);
    }
    return ret;
  } else if (arrayp (value)) {
    array ret = ({});
    foreach (value, mixed val) {
      ret += ({ decode_objects (val) });
    }
    return ret;
  } else if (objectp (value)) {
    if (value->is_val_null || value->is_val_false)
      return 0;
    else if (value->is_val_true)
      return 1;
    error ("Unrecognized object.\n");
  } else {
    return value;
  }
}


typedef function(object:mixed) ExtFieldResolver;

ExtFieldResolver
get_base_field_resolver(void|mapping(string:array(string)) dbobj_field_hints)
{
  return lambda (object obj) {
           if (dbobj_field_hints && obj->rec_from_get_ext) {
             mapping(string:int) get_fields;
             if (string object_type = obj->get_ext ("object_type")) {
               constant base_fields = ([ "unique_object_id": 1,
                                         "object_type": 1,
                                         "id": 1 ]);

               array(string) obj_hints =
                 dbobj_field_hints[object_type] || ({});
               get_fields =
                 base_fields +
                 mkmapping (obj_hints, allocate (sizeof (obj_hints), 1));
             } else {
               error ("%O didn't return an object_type.\n");
             }

             return obj->rec_from_get_ext (get_fields);
           } else if (obj->get_client_rec) {
             return obj->get_client_rec();
           } else if (obj->is_rep_label_wrapper) {
             return obj->resolve();
           }
           error ("Couldn't handle object %O.\n", obj);
         };
}

//! Resolver to be used in InDesign context, i.e. when path references
//! are to be resolved via a file share.
REP.DB.ExtFieldResolver
get_id_field_resolver(void|mapping(string:array(string)) dbobj_field_hints)
{
  REP.DB.ExtFieldResolver default_resolver =
    REP.DB.get_base_field_resolver (dbobj_field_hints);

  return lambda (object obj)
         {
           if (obj->is_rep_ext_field_ref) {
             return obj->fileshare_field_path();
           } else {
             return default_resolver (obj);
           }
           error ("Couldn't handle object %O.\n", obj);
         };
}

// FIXME: Handle reference cycles.
mixed resolve_ext_fields (mixed value, ExtFieldResolver resolver)
{
  if (stringp (value) || intp (value) || floatp (value)) {
    return value;
  } else if (arrayp (value) || mappingp (value)) {
    return map (value, resolve_ext_fields, resolver);
  } else if (multisetp (value)) {
    return map ((array)value, resolve_ext_fields, resolver);
  } else if (objectp (value)) {
    if (value->is_val_null || value->is_val_boolean) {
      return value; // Handled by JSON encoder.
    }

    return resolve_ext_fields (resolver (value), resolver);
  } else if (functionp (value)) {
    return resolve_ext_fields (value(), resolver);
  }

  error ("Don't know how to handle %O\n", value);
}
