// $Id: 06dae3514653b97201f94461461072215c77c12f $

//! Object representing a page group version.
//!
//! A page group is based on a layout file, containing zero or more
//! pages (although one with zero pages in it is admittedly odd and
//! unlikely to occur). The page group is versioned, so the layout
//! file will not ever change once it has been written.
//!
//! Most other attributes are also versioned, and they may be changed
//! only as long as this page version has not "become current" by
//! being pointed to by page_groups.cur_pgversion_id (with the
//! exception of adding more pages - see below).
//!
//! The pages in the page group are represented by a number of
//! @[REP.Page] objects. The basic principle is that the @[REP.Page]
//! objects track the pages in the layout file, but that tracking is
//! not strict:
//!
//! @ul
//! @item
//!   There might be more @[REP.Page] objects than layout file pages.
//!   The excess page objects are called "unconnected" and have NULL
//!   ext_page_id's. By consequence, they are missing things like
//!   previews.
//!
//! @item
//!   When a layout file has not yet been processed by the preview
//!   generators, it might have more pages than there are @[REP.Page]
//!   objects. That is considered a temporary inconsistency that is
//!   fixed as soon as the layout file is processed and the new pages
//!   are discovered.
//!
//!   This means that page versions might get additional @[REP.Page]
//!   objects after they have been made current, or even have been
//!   superseded. Other additions of extra pages (e.g. for planning
//!   purposes) should only be done before the page group version is
//!   current.
//!
//!   The extra pages are always discovered before the processed
//!   layout file appears. I.e. all @[REP.Page] objects are present
//!   when the notify event on @[REP.Storage.LAYOUT_FILE] is
//!   triggered.
//!
//!   In addition to the pages themselves, there are also some other
//!   fields, e.g. markup_error, whose values are calculated from the
//!   processing of the layout file, and they may also change in a
//!   live version as described above.
//! @endul
//!
//! @[REP.PGVersion] objects map to rows in the page_versions table,
//! but it is also used to query and update the unversioned data in
//! the page_groups table.
//!
//! This class handles the storage of all files associated with page
//! groups. See @[REP.Storage] for general file storage principles.
//! Page file specifics:
//!
//! @ul
//! @item
//!   Layout files can be of different MIME types, and are told apart
//!   by extension (see @[REP.Storage.layout_filenames_by_mime]) of
//!   the unprocessed layout file. There is at most one type of layout
//!   file per page version.
//!
//! @item
//!   A page version might lack a layout file, and in that case its
//!   type is naturally undecided. When a page version has a layout
//!   file, it will not ever change the layout file type (follows from
//!   the above).
//!
//! @item
//!   The MIME type of the layout file also affects the extensions for
//!   the deconstruction and reconstruction files.
//!
//! @item
//!   A layout file may be either unprocessed
//!   (@[REP.Storage.UNPROC_LAYOUT_FILE]) or processed
//!   (@[REP.Storage.LAYOUT_FILE]). When a layout file is uploaded, it
//!   is unprocessed. The layout module reads it, generates page
//!   previews, ensures the @[REP.Page]s are in order, and maybe even
//!   cleans it up in some ways. After that is done, the processed
//!   file (which might be identical to the unprocessed one) is
//!   written. Note: An unprocessed layout file can only be uploaded
//!   to a @[REP.PGVersion] that isn't live, i.e. one that has not
//!   been made current.
//!
//! @item
//!   A layout file may skip the unprocessed stage if no processing by
//!   the layout module is required for it, i.e. there might be a
//!   @[REP.Storage.LAYOUT_FILE] without a corresponding
//!   @[REP.Storage.UNPROC_LAYOUT_FILE]. In that case things like
//!   up-to-date @[REP.Page] objects and previews must be ensured in
//!   some other way, though.
//!
//! @item
//!   Page previews are naturally page specific, and there might be
//!   several for a multipage group. Since page previews are generated
//!   for the unprocessed layout file, there is a dependency from
//!   @[REP.Storage.LAYOUT_FILE] to all @[REP.Storage.PAGE_PREVIEW]
//!   files, but not for other preview sizes.
//!
//! @item
//!   There are several different page preview sizes. Every page
//!   typically has all of them, but they can be created in any order.
//!
//! @item
//!   A page version might have both one group pdf file and
//!   page-specific pdf files. If that happens then there is no
//!   difference between them for the specific pages.
//!
//! @item
//!   Box previews are document specific, and are given unique names
//!   by using the document id integers.
//! @endul

#include "rep.h"

inherit REP.DBObject;

constant is_rep_pgv = 1;
//! Nonzero recognition constant.

constant class_name = "REP.PGVersion";
constant table_name = "page_versions";

constant avail_fields = ([
  "page_version_id": 1,
  "storage_counter": 1,
  "page_group_id": 1,
  "page_status_id": 1,
  "page_type_id": 1,
  "is_deleted": 1,
  "delete_at": 1,
  "date_created": 1,
  "user_id": 1,
  "user_handle": 1,
  "user_fullname": 1,
  "comment": 1,
  "markup_error": 1,		// Property. Boolean or unset.
  "preflight_results": 1,       // Property. Mapping or unset.
  "story_slots": 1,		// Property.
  "indesign_version": 1,        // Property. String or unset
  "doc_layout_info": 1,         // Property. Excludes page-specific info.

  //"pages": 1,
  // Internally handled property. Stores the pages belong to this page
  // group version on this form:
  //
  // ([4711: (["page_id": 4711, ...]), ...])
  //
  // When a page group version becomes current, the pages table is
  // updated to match the content of that property. This means the
  // "pages" property is the authoritative source.

  "user_storage_counter": 1,
  // Property. Iff set, the stored layout files in this version will
  // be recreated automatically in case there's a content conflict on
  // page file uploads. An example of this is page versions that are
  // the result of a repagination - such versions will be overwritten
  // since the user's uploaded file will be paginated automatically in
  // the UNPROC -> PROC stage.

  "text_flow_links": 1,
  // Property. Stores the text links deconstructed from the layout
  // file for this version on this form:
  //
  // (["b80aaa78-c8fb-453c-ac60-48761d1f912e": ([
  //     "uuid": "b80aaa78-c8fb-453c-ac60-48761d1f912e",
  //     "doc_id": 17,
  //     ...]),
  //   ...])
  //
  // The value mappings are the same as the record mappings i
  // TextFlowLinks.
  //
  // Use cur_text_flow_links() to access these through the proper
  // TextFlowLink objects.

  "file_md": 1,
  // Property. Stores metadata about files in this object on this form:
  //
  // ([ "filename": ([ index: value,
  //                   index2: value ]) ])
  // See the avail_file_md_fields mapping for available fields.
  // The mapping index is the filename as generated by get_page_filepath().
  "user_properties": 1, // Property.

  //"placements_bucket_id": 1, // Property. Only accessed by
                               // DBObject::internal_get_bucket
]);

// Fields that should be shared only between PGVersions having
// identical storage counters. These fields are derived from the files
// in the storage directory.
constant shared_storage_fields = ([
  "markup_error": 1,
  "preflight_results": 1,
  "pages": 1,
  "user_storage_counter": 1,
  "text_flow_links": 1,
  "file_md": 1,
  "indesign_version": 1,
  "placements_bucket_id": 1,
]);

constant col_defaults = ([
  "page_status_id": 1,
  "page_type_id": 1,
  "is_deleted": 0,
  "user_id": Val.null,
  "user_handle": Val.null,
  "user_fullname": Val.null,
  "comment": "",
]);

constant avail_ext_fields = ::avail_ext_fields + avail_fields +
  ([ "page_status"               : 1,
     "package_file_downloadable" : 1,
     "file_urls"                 : 1,
     "preflight_results_txt"     : 1,
     "permission"                : 1, ]) -
  ([ "story_slots" : 1,		// Maybe not necessary to exclude.
     "pages"       : 1,
     "text_flow_links": 1,   // Exclude unformatted raw data.
     "preflight_results": 1, // Exclude unformatted raw data.
     "doc_layout_info": 1,   // Exclude unformatted raw data.
  ]);

// Cacheable versioned fields
mapping(string:int(1..1)) cacheable_fields = avail_fields +
  ([ "preflight_results_txt": 1, // Cache is automatically wiped when
				 // preflight_results is changed. Cache wiped
     "file_urls": 1,             // by set_fields() & notify_page_file_added().
  ]) -
  ([ "story_slot": 1,
     "pages": 1,
     "text_flow_links": 1,
     "preflight_results": 1,
     "doc_layout_info": 1,
  ]);

// FIXME: Handle the page categories as fields.

// Fields that are unversioned, i.e. stored in the page_groups record.
constant avail_unver_fields = ([
  "page_group_id": 1,
  "edition_id": 1,
  "cur_pgversion_id": 1,
  "page_group_uuid": 1,
  "is_edit_locked": 1,
  "edit_user_id": 1,
  "edit_user_handle": 1,
  "edit_user_fullname": 1,
  "is_deleted": 1,
  "delete_at": 1,
  "edit_lock_expiry": 1, // Property. Not used when is_edit_locked is false.
  "admgr_pending_jobs": 1, // Property used by the ad system manager.
  "deferred_page_updates": 1, // Property. See add_deferred_page_update.
  "user_properties": 1,  //  Property.
]);

// Cacheable unversioned fields, cached in pg_volatile->unver_fields_cache.
constant cacheable_unver_fields = ([
  "page_categories": 1 // Cache wiped by set_page_group_category().
]);

constant unver_col_defaults = ([
  "is_edit_locked": 0,
  "edit_user_id": Val.null,
  "edit_user_handle": Val.null,
  "edit_user_fullname": Val.null,
  "is_deleted": 0,
]);

constant avail_unver_ext_fields = ([
  "is_edit_locked":1,	//  0 for unlocked, !0 for locked (with 2 meaning busy
  			//  in a server action)
  "edit_user_id":1,
  "edit_user_handle":1,
  "edit_user_fullname":1,
  "ad_pending_updates":1,
  "page_categories": 1
]);

constant obsolete_fields = ([
  "crc32_layout": 1,
  "crc32_unproc_layout": 1,
  "md5_layout": 1,
  "md5_unproc_layout": 1,
]);

// File metadata fields.
constant avail_layout_md_fields = ([
  "md5": 1,
  "crc32": 1,
  "sha256": 1,
]);

constant avail_pdf_md_fields = ([
  "preset": 1,
  "preset_hash": 1,
  "pdf_settings_gen": 1,
]);

constant avail_package_md_fields = ([
  //  Depend on same generation as for PDF files. Depending on package config
  //  they will include PDFs as well, but this generation counter dependency
  //  is used regardless as a universal method to invalidate packages.
  "pdf_settings_gen": 1,
]);

constant avail_jpeg_md_fields = ([
  "dims": 1,   //  ({ width, height })
]);

constant avail_file_md_fields = ([
  REP.Storage.UNPROC_LAYOUT_FILE:   avail_layout_md_fields,
  REP.Storage.LAYOUT_FILE:          avail_layout_md_fields,

  REP.Storage.PDF_PAGE_PREVIEW:     avail_pdf_md_fields,
  REP.Storage.PDF_PAGE_PREVIEW_LR:  avail_pdf_md_fields,
  REP.Storage.PDF_GROUP_PREVIEW:    avail_pdf_md_fields,
  REP.Storage.PDF_GROUP_PREVIEW_LR: avail_pdf_md_fields,

  REP.Storage.PAGE_PREVIEW:         avail_jpeg_md_fields,
  REP.Storage.PAGE_PREVIEW_BIG:     avail_jpeg_md_fields,
  REP.Storage.PAGE_PREVIEW_SMALL:   avail_jpeg_md_fields,

  REP.Storage.PACKAGE_FILE:         avail_package_md_fields
]);

this_program parent_version;
//! The @[REP.PGVersion] that this one is based on, if any. Only set
//! for versions that have not been made current (see
//! @[DB.make_pgv_current]). Note that this field is not persistent,
//! so it won't be set for records read from the database.

int deferred_purge;
//! Non-persistent deferred purge marker. The page version and its
//! associated files will get permanently deleted as soon as there
//! are no more references left to the version (or its files).
//! @seealso
//!   @[delete()]

mapping pg_volatile;
//! A mapping that is shared between all @[PGVersion] objects for the
//! same page group. It exists for as long as any @[PGVersion] object
//! for the page group exists in memory.
//!
//! The mapping always gets zapped by @[DB.DBMaintenanceLock], so it's
//! safe to store @[DBObject]s in it (remember to @[DBObject.refresh]
//! them, though).

//! @ignore
DO_IF_DEBUG (int is_live = 1;);
//! @endignore

protected void create (mapping(string:mixed) rec_from_db, REP.ObjectTable tbl)
{
  ::create (rec_from_db, tbl);

  if (rec_from_db) {
    // Note: The following assertions may cause SQL queries and
    // thereby deadlocks - see DBObject.create() /
    // CachedTable.cached_get(). Run them in a separate thread to avoid
    // that. The deadlocks have mostly been observed while running the
    // testsuite.
#ifdef DEBUG
    REP.dispatch(lambda()
		 {
		   ASSERT_IF_DEBUG (page_group_rec());
		   ASSERT_IF_DEBUG (edition());
		 });
#endif

    int page_group_id = rec_from_db->page_group_id;
    ASSERT_IF_DEBUG (page_group_id);
    mapping(int:mapping) pg_volatiles = tbl->print_db->pg_volatiles;
    pg_volatile =
      (pg_volatiles[page_group_id] || (pg_volatiles[page_group_id] = ([])));
  }
}

protected class PGVersionCorpse (REP.PrintDBModule print_db,
				 mapping(string:mixed) rec,
				 string storage_path)
// This object is created from destroy() and scheduled to be executed
// by a handler thread, to do the db stuff we cannot do there.
{
  void `() ()
  {
#if 0
    werror ("delayed deferred purge for record %O\n", rec->page_version_id);
#endif

    // Check that print_db and print_db->db_tables still exist,
    // because we can get called after the module instance has been
    // stopped or destructed. In those cases there's not much to do
    // but leave the garbage to the periodic gc.
    if (mapping(string:REP.DBTable) db_tables =
	print_db && print_db->db_tables) {

      REP.REPSessionTracker rst = REP.REPSessionTracker (print_db);
      int storage_counter = rec->storage_counter;
      if (REP.ObjectTable pv_tbl = db_tables->page_versions) {
	pv_tbl->cached_remove (rec->page_version_id);

	// Invalidate the page lookup cache in associated page slots.
	foreach (values (rec->pages), mapping(string:mixed) page_rec) {
	  if (REP.PageSlot slot =
	      REP.DB.page_slot_by_id (page_rec->page_slot_id, REP.RETURN_ZERO))
	    slot->invalidate_pages();
	}

	if (!pv_tbl->select1("count(page_version_id)",
			     "storage_counter = " + storage_counter)[0]) {
	  // The storage is not in use by anyone else.
	  // Clean up the storage as well.
	  foreach(print_db->scan_db_directory(storage_path, 0x7fffffff, 1),
		  string path) {
#if 0
	    werror ("cleaning up file %O\n", path);
#endif
	    print_db->remove_db_file(path, REP.LOG_ERROR);
	  }
	}
      }
    }

    // FIXME: Remove the page group too if it's marked for deletion
    // and have no versions left. Otherwise we'll get heaps of such
    // page groups for deleted placeholders.
  }
}

protected void destroy (int reason)
{
#ifdef DEBUG
  IsDestructing destr = IsDestructing (reason);
#endif
#if 0
  werror("%O->destroy()\n"
	 "  %O, %O\n", this_object(), deferred_purge, tbl->print_db);
#endif

  //  Waiting promises will never be fulfilled by a version that is
  //  destructed so cancel them.
  cancel_all_waiting_promises();

  if (deferred_purge && id()) {
    //  Execute in call_out() to avoid recursive mutex locks when this
    //  destroy callback is invoked during modification of the background_run
    //  queue.
    PGVersionCorpse pgv_corpse =
      PGVersionCorpse(tbl->print_db, rec, get_storage_path());
    call_out(roxen.background_run, 0, 0, pgv_corpse);
  }
}

protected string _sprintf (int flag, mapping(string:mixed) args)
{
  // Try to make a good page description by looking up the page names
  // through the page slot objects. Have to be tolerant to nonexisting
  // objects in this function, and be careful to only do lookups in
  // ram cached state.
  array page_descrs = ({});
  if (rec->pages) {
    page_descrs = allocate (sizeof (rec->pages));
    if (args->flag_left) {
      // Use %-O to force page and slot lookup.
      array(REP.Page) pages = this_program::pages();
      foreach (pages; int i; REP.Page page) {
	if (REP.PageSlot slot = page->page_slot())
	  page_descrs[i] = slot->page_name();
	else
	  page_descrs[i] = "<" + page->id() + ">";
      }
    }
    else {
      array(mapping(string:mixed)) pages = values (rec->pages);
      sort (pages->doc_order, pages);
      REP.ObjectTable page_slot_tbl =
	REP.get_table ("page_slots", REP.RETURN_ZERO);
      foreach (pages; int i; mapping(string:mixed) page_rec) {
	if (int slot_id = page_slot_tbl && page_rec->page_slot_id)
	  if (REP.PageSlot slot = page_slot_tbl->get_from_cache (slot_id)) {
	    page_descrs[i] = slot->page_name();
	    continue;
	  }
	page_descrs[i] = "<" + page_rec->page_id + ">";
      }
    }
  }

  int pgv_id = rec->page_version_id;
  int cur_pgv_id = _page_group_rec && _page_group_rec->cur_pgversion_id;

  return flag == 'O' &&
    sprintf ("REP.PGVersion(%O%s [%s], pg %O%s, ed %s%s)" + OBJ_COUNT,
	     pgv_id,
	     !_page_group_rec ? "" :
	     pgv_id == cur_pgv_id ? " cur" :
	     pgv_id > cur_pgv_id ? " later" :
	     " live",
	     page_descrs * ",",
	     rec->page_group_id,
	     _page_group_rec && _page_group_rec->is_deleted ? " del" : "",
	     _page_group_rec ? (string) _page_group_rec->edition_id : "?",
	     deferred_purge ? ", purge" : rec->is_deleted ? ", del" : "");
}

//  Cached copy of edition()->publ_date_stamp() that's set in create().
//  Might not reflect updates in the edition but we accept that.
protected int cached_edition_publ_ts;

float cache_entry_multiplier()
{
  //  A version that is deleted or is non-current should be evicted faster
  if (rec->is_deleted)
    return CACHE_ENTRY_MULTIPLIER_OLD;
  int pgv_id = rec->page_version_id;
  if (int cur_pgv_id = _page_group_rec && _page_group_rec->cur_pgversion_id) {
    if (pgv_id < cur_pgv_id)
      return CACHE_ENTRY_MULTIPLIER_OLD;
  }

  //  Versions associated to editions whose publication dates have passed
  //  should also not be cached as aggressively. Publication date may be at
  //  00:00 today but still be valid so we subtract another 24 hours to get
  //  a safe margin.
  if (cached_edition_publ_ts && (cached_edition_publ_ts < (time(1) - 86400)))
    return CACHE_ENTRY_MULTIPLIER_OLD;

  return 1.0;
}

//! Returns the names of the page slots that the pages in this
//! PGVersion are assigned to, intended as a display string.
//!
//! @param include_deleted
//!   Deleted slots are skipped unless this flag is set.
string page_names(void|int(0..1) include_deleted)
{
  array(string) names = ({ });
  foreach (pages(), REP.Page page) {
    if (REP.PageSlot slot = page->page_slot()) {
      if (!slot->get ("is_deleted")) {
	names += ({ slot->page_name() });
      } else if (include_deleted) {
	names += ({ slot->page_name() + " (deleted)" });
      }
    }
  }
  return REP.Utils.group_page_nums(names);
}

string obj_path (void|REP.Publication|REP.Edition base)
{
  REP.Edition ed = edition();
  string ed_action = ed != base &&
    ed->obj_path (base && base->is_rep_publication && base);

  array(REP.Page) pgs = pages();
  string pgs_action = sizeof (pgs) == 1 ?
    "p" + pgs[0]->obj_path (this) :
    "pp(" + pgs->obj_path (this) * "," + ")";

  if (ed_action)
    return ed_action + "/" + pgs_action;
  else
    return pgs_action;
}

REP.Edition parent()
//! Alias for @[edition()].
{
  return edition();
}

int access_perm()
//! Returns the permission on this PGV for the current REP
//! session. Returns @[AC.PERM_WRITE] for PGVs that have no
//! configured ppoint and for server internal sessions.
//!
//! The access permissions are currently inherited from the
//! @[Edition].
{
  int status_perm = REP.AC.access_perm_page_status (page_status());
  // The page status ppoints are documented to have no semantic
  // meaning for AC.PERM_READ, so users disallowed from writing will
  // typically have AC.PERM_NONE on them. Elevate to PERM_READ for
  // now to achieve DWIM.
  if (status_perm == AC.PERM_NONE) status_perm = AC.PERM_READ;

  return min (status_perm, parent()->access_perm());
}

// This mapping is part of the CachedTable cache for the page_groups table.
protected mapping(string:mixed) _page_group_rec;

protected mapping(string:mixed) page_group_rec()
{
  if (!_page_group_rec) {
    ASSERT_IF_DEBUG (rec->page_group_id);
    _page_group_rec = REP.DB.page_groups_table()->
      cached_get (rec->page_group_id, REP.LOG_ERROR);
    if (!_page_group_rec)
      // Tables are seriously funky if this happens, but we try to
      // avoid internal server errors anyway. We're careful to not
      // cache this dummy mapping, though.
      return ([
	"page_group_id": rec->page_group_id,
	0: 1,		   // A marker to recognize this dummy mapping.
      ]);

    //  Cache the edition's publication timestamp locally so our RAM cache
    //  multiplier callback can access it without a RST. It might be wrong
    //  if the date changes but that's ok.
    ASSERT_IF_DEBUG(_page_group_rec->edition_id);
    if (REP.Edition ed =
        REP.DB.edition_by_id(_page_group_rec->edition_id, REP.RETURN_ZERO)) {
      cached_edition_publ_ts = ed->publ_date_stamp();
    }
  }
  return _page_group_rec;
}

this_program current_pgv()
//! Might return zero only for new pages which have had no current
//! page version yet.
{
  return REP.DB.current_pgv_by_pg_id (rec->page_group_id);
}

//! Returns the most recent version that has a @[LAYOUT_FILE]. May
//! return zero if no version has been made current yet.
this_program current_layout_pgv()
{
  REP.PGVersion cur_pgv = current_pgv();
  if (!cur_pgv)
    return UNDEFINED;

  // Easy case first.
  if (cur_pgv->get_existing_page_filepath (REP.Storage.LAYOUT_FILE)) {
    return cur_pgv;
  }

  // Now look in all versions.
  foreach (all_versions(), REP.PGVersion pgv) {
    if (pgv->id() <= cur_pgv->id() &&
        pgv->get_existing_page_filepath (REP.Storage.LAYOUT_FILE)) {
      return pgv;
    }
  }
}

//! Returns the most recent PGVersion that doesn't have markup errors.
this_program current_template_pgv()
{
  REP.PGVersion cur_pgv = current_pgv();
  // Easy case first. This will match if the current PGV has a
  // LAYOUT_FILE without markup errors.
  if (cur_pgv->is_valid_template_pgv())
    return cur_pgv;

  // Now look in all versions.
  foreach (all_versions(), REP.PGVersion pgv) {
    if (pgv->id() <= cur_pgv->id() && pgv->is_valid_template_pgv()) {
      return pgv;
    }
  }
}

//! Returns 1 if this pgv has a LAYOUT_FILE but no markup
//! errors. Note: doesn't imply that this is the most recent such
//! pgv. Also used as a predicate in @[TemplateDef.is_current].
int(0..1) is_valid_template_pgv()
{
  return !get ("markup_error") && is_valid_file (REP.Storage.LAYOUT_FILE);
}

int(0..1) is_current()
//! See @[DBObject.is_current] for semantics.
{
  return !get_unver ("is_deleted") && !get ("is_deleted") &&
    this == current_layout_pgv() &&
    !is_scratch() && access_perm() && parent()->is_current();
}

array(REP.PGVersion) all_versions (void|int(1..) limit,
				   void|int include_deleted)
{
  array(int) all_pgv_ids = pg_volatile->all_pgv_ids;
  array(int) nondeleted_pgv_ids = pg_volatile->nondeleted_pgv_ids;
  if (!all_pgv_ids || !nondeleted_pgv_ids) {
    all_pgv_ids = ({});
    nondeleted_pgv_ids = ({});
    foreach (tbl->select ("page_group_id = " + rec->page_group_id,
			  ({ "page_version_id", "is_deleted" }),
			  0,
			  0,
			  "ORDER BY page_version_id DESC");
	     int i;
	     mapping(string:mixed) row) {
      all_pgv_ids += ({ row->page_version_id });
      if (!row->is_deleted)
	nondeleted_pgv_ids += ({ row->page_version_id });
    }

    pg_volatile->all_pgv_ids = all_pgv_ids;
    pg_volatile->nondeleted_pgv_ids = nondeleted_pgv_ids;
  }

  array(int) final_pgv_ids;

  if (include_deleted) {
    if (limit)
      final_pgv_ids = all_pgv_ids[..limit-1];
    else
      final_pgv_ids = all_pgv_ids;
  } else {
    if (limit)
      final_pgv_ids = nondeleted_pgv_ids[..limit-1];
    else
      final_pgv_ids = nondeleted_pgv_ids;
  }

  return tbl->object_get_multi (final_pgv_ids);
}

REP.Edition edition()
//! Won't return zero.
{
  return REP.DB.edition_by_id (page_group_rec()->edition_id);
}

//! Fetch all categories for this PGVersion.
//!
//! Use with caution as it isn't cachable at this time!
//!
array(mapping(string:mixed)) categories()
{
  SqlTools.SqlTable pc_tbl = REP.DB.page_categories_table();
  SqlTools.SqlTable.Result res = pc_tbl->select("page_group_id = " +
						rec->page_group_id,
						UNDEFINED,
						UNDEFINED,
						UNDEFINED,
						"ORDER BY is_main DESC");
  return res->get_array();
}

string page_status()
{
  return rec->page_status_id &&
    REP.DB.get_page_status (rec->page_status_id);
}

string page_type()
//! Won't return zero.
{
  return REP.DB.get_page_type (rec->page_type_id);
}

int is_placeholder()
//! Returs nonzero if this page group is a placeholder, i.e. that it
//! satisfies all these conditions:
//!
//! @ul
//! @item
//!   Has at most one page which might be connected to a page group.
//! @item
//!   Has no layout file.
//! @item
//!   Has the default page status and page type.
//! @item
//!   Has no page categories assigned.
//! @item
//!   Has no story appearances assigned.
//! @item
//!   Has no earlier versions.
//! @item
//!   Has no edit lock.
//! @item
//!   Has no page description.
//! @endul
//!
//! @note
//! The conditions above might need tuning, e.g. allowing changed page
//! status and/or history with only page status changes.
{
  // FIXME: This function is slower than one might expect since it
  // makes several db queries. Consider adding a cache when all the
  // tables are accessed through the object layer.

  if (rec->page_status_id != col_defaults->page_status_id) return 0;
  if (rec->page_type_id != col_defaults->page_type_id) return 0;

  if (page_group_rec()->is_edit_locked) return 0;

  array(REP.Page) pgs = pages();
  if (sizeof (pgs) > 1) return 0;
  if (sizeof (pgs) == 1 && pgs[0]->get ("description") != "") return 0;
  if (layout_mime_type()) return 0;

  if (sizeof (stories())) return 0;
  if (REP.DB.page_versions_table()->select1 (
	"COUNT(*)", "page_group_id=" + rec->page_group_id)[0] > 1) return 0;
  if (REP.DB.page_categories_table()->select1 (
	"COUNT(*)", "page_group_id=" + rec->page_group_id)[0]) return 0;

  return 1;
}

int is_placed()
//! Returns nonzero if at least one page is getting in the printed
//! edition, i.e. is in a non-deleted slot. This is the inverse of
//! @[is_scratch], except that both return zero for deleted pgvs.
{
  if (page_group_rec()->is_deleted) return 0;
  // FIXME: This function is used as a predicate. It might need a cache.
  foreach (pages(), REP.Page page)
    if (REP.PageSlot slot = page->page_slot())
      if (!slot->get ("is_deleted"))
	return 1;
  return 0;
}

int is_scratch()
//! Returns nonzero if all pages are in the scratch area, i.e. none is
//! in a non-deleted slot. This is the inverse of @[is_placed], except
//! that both return zero for deleted pgvs.
{
  if (page_group_rec()->is_deleted) return 0;
  // FIXME: This function is used as a predicate. It might need a cache.
  foreach (pages(), REP.Page page)
    if (REP.PageSlot slot = page->page_slot())
      if (!slot->get ("is_deleted"))
	return 0;
  return 1;
}

bool is_shadow()
{
  //  If any of the PGV pages is shadowed we consider the whole group to be
  //  shadowed. We don't exclude deleted slots since the shadow parent ID
  //  should always be cleared for deleted slots.
  array(REP.PageSlot) pss = pages()->page_slot() - ({ 0 });
  return sizeof(pss->get("shadow_parent_id") - ({ 0 }) ) > 0;
}

REP.PGVersion|int(0..0) get_synced_ads_master()
{
  //  A subset of shadowed PGVs may also have the Ads layer synced. This is
  //  determined by checking for identical ext_source_id values in all slots.
  if (is_shadow()) {
    //  Resolve slots first to only get page objects for non-empty slots
    array(REP.PageSlot) shadow_pss = pages()->page_slot() - ({ 0 });
    array(REP.Page) shadow_pgs = shadow_pss->page();
    if (!sizeof(shadow_pss->get("shadow_parent_id") - ({ 0 }) ))
      return 0;
    array(REP.PageSlot) master_pss = shadow_pss->shadow_top_parent() - ({ 0 });
    array(REP.Page) master_pgs = master_pss->page();
    if (!sizeof(shadow_pgs) || !sizeof(master_pgs))
      return 0;

    REP.PGVersion master_pgv = master_pgs[0]->pgv();
    return
      equal(master_pgs->get("ext_source_id"),
            shadow_pgs->get("ext_source_id")) &&
      master_pgv;
  }
  return 0;
}


protected void flush_all_versions_cache()
{
  m_delete (pg_volatile, "all_pgv_ids");
  m_delete (pg_volatile, "nondeleted_pgv_ids");
}

void invalidate_is_current_low()
{
  if (REP.DBObject/*REP.Bucket*/ bucket =
      internal_get_bucket_no_create ("placements_bucket_id"))
    //  Pass hint that no non-current object will become
    //  current in this invalidation.
    bucket->invalidate_is_current(1);
}

void invalidate_is_current()
{
  invalidate_is_current_multi( ({ this }) );
}

void invalidate_is_current_multi(array(REP.PGVersion) pgvs)
{
  REP.dispatch (lambda() {
                  foreach (pgvs, REP.PGVersion pgv)
                    pgv->invalidate_is_current_low();
                });
}

protected void invalidate_is_current_multi_cb()
{
  invalidate_is_current_multi(all_versions());
}

void check_current_layout_invalidation()
{
  // Invalidate the "current" state in buckets belonging to this
  // version (the new one) and the version that was previously
  // current, if any.
  if (this == current_layout_pgv()) {
    REP.get_session()
      ->register_uniq_session_done_callback(invalidate_is_current_multi_cb);
  }
}

void db_update_hook (string action,
                     void|mapping(string:mixed)|REP.DBObject arg)
{
  if (action == "new_version") {
    check_current_layout_invalidation();
    perform_pending_operations (current_pgv());
  }

  ::db_update_hook (action, arg);
}

void ver_record_hook (string action, this_program record, mixed ... args)
{
  switch (action) {
  case "create":
    flush_all_versions_cache();
    break;
  case "modify":
    mapping(string:int) changed_fields = sizeof (args) && args[0];
    if (!changed_fields || changed_fields["is_deleted"]) {
      flush_all_versions_cache();
      check_current_layout_invalidation();
    }
    break;
  }
}

void unver_record_hook (string action, mixed record, mixed ... args)
{
  multiset changed_fields = sizeof (args) && args[0];
  if (!changed_fields ||
      (!zero_type (changed_fields->is_edit_locked) &&
       !changed_fields->is_edit_locked)) {
    execute_deferred_updates();
  }
}

protected void internal_set_raw (mapping(string:mixed) vals)
{
  // In live versions we only allow changing the is_deleted flag - it
  // isn't versioned. Also allow other fields that result from the
  // deconstruct.
  ASSERT_IF_DEBUG (!is_live ||
		   !(sizeof (vals /*%O*/ - (["is_deleted": 1,
					     "delete_at": 1,
					     "markup_error": 1,
					     "preflight_results": 1,
					     "file_md": 1,
                                             "placements_bucket_id": 1,
					     "user_properties": 1,
					     "indesign_version": 1,
                                             "doc_layout_info": 1 ]))),
                   vals);
  ASSERT_IF_DEBUG ((zero_type (vals->page_group_id) &&
		    zero_type (vals->storage_counter)) || !id());
  ASSERT_IF_DEBUG (zero_type (vals->markup_error) ||
		   (<0, 1>)[vals->markup_error /*%O*/], vals->markup_error);
  ::internal_set_raw (vals);
  if (!rec->is_deleted) deferred_purge = 0;

  if (!zero_type (vals->is_deleted)) // ver_record_hook gets called
    flush_all_versions_cache();      // only after the session has
				     // finished, so we need to clear
				     // the cache here also to get
				     // proper semantics within a
				     // session.

  ASSERT_IF_DEBUG (zero_type (vals->page_group_id) || page_group_rec());
}

// Only to be called from PGVersion.commit() (in a different PGVersion
// instance). Updates fields that should be shared between PGVersion
// objects having identical storage_counters.
void update_shared_storage_fields (mapping(string:mixed) vals)
{
  ASSERT_IF_DEBUG (sizeof (vals /* %O */) ==
		   sizeof (vals & shared_storage_fields),
		   vals);

  // Changes in the "pages" mapping must be handled separately, see code below.
  mapping(int:mapping(string:mixed)) updated_pages = m_delete (vals, "pages");

  DisableAutoCommit dac = DisableAutoCommit();
  // Call the inherited internal_set_raw() to bypass unwanted
  // assertions in our own internal_set_raw().
  ::internal_set_raw (vals);

  if (!zero_type (vals->file_md)) {
    foreach (pages(), REP.Page page) {
      page->clear_client_rec_cache();
    }
  }

  if (!zero_type (vals->text_flow_links)) {
    // Clear text flow link caches. Code duplicated from set_text_flow_link.
    tflf_by_doc_id = 0;
    if (current_pgv() == this) {
      // The pg_volatile->tfl_pgv_id check in cur_text_flow_links avoids
      // using stale caches in all cases except when changing the
      // current pgv, so we have to zap the caches now.
      // vvv Relying on the interpreter lock from here.
      m_delete (pg_volatile, "tfl_pgv_id");
      m_delete (pg_volatile, "tfl_by_uuid");
      // ^^^ Relying on the interpreter lock to here.
    }
    edition()->invalidate_text_chains();
  }

  if (updated_pages) {
    mapping(int:mapping(string:mixed)) removed_pages =
      rec->pages - updated_pages;
    array(mapping(string:mixed)) new_pages = ({});

    foreach (updated_pages; int page_id; mapping(string:mixed) rec) {
      if (REP.Page p = page_by_id (page_id, REP.RETURN_ZERO)) {
	p->set_fields (rec - ([ "page_id": 1 ]));
      } else {
	new_pages += ({ rec });
      }
    }
    if (sizeof (new_pages))
      add_pages (new_pages);
    if (sizeof (removed_pages))
      remove_pages (map (indices (removed_pages), page_by_id));
  }

  commit (1);
  destruct (dac);
}

// The no_shared_update argument is provided by
// update_shared_storage_fields to avoid unwanted cascade effects.
this_program commit (void|int no_shared_update)
{
  // doc_order should always be an unbroken strictly increasing
  // sequence starting from 1.
  ASSERT_IF_DEBUG (!rec->pages /*%O*/ ||
		   equal (sort (values (rec->pages)->doc_order),
			  map (indices (allocate (sizeof (rec->pages))),
			       `+, 1)),
		   rec->pages);

  int infant = !id();
  int prev_storage_counter = rec->storage_counter;

  mapping(string:mixed) changed_shared_fields =
    !infant && !no_shared_update &&
    ((shared_storage_fields & dirty_fields) & rec);

  int flush_cache = 0;
  if (dirty_fields->page_group_id) // New version. We must flush the
    flush_cache = 1;               // versions cache after commit.

  if (rec->page_group_id && current_pgv() == this)
    internal_update_pages (dirty_fields->pages);
  ::commit();
  if (infant && pages_by_page_id) {
    add_in_use_pages (pages_by_page_id);
    values (pages_by_page_id)->internal_set_page_version (this);
  }

  if (infant && (rec->storage_counter == prev_storage_counter)) {
    //  Any associated placement bucket should have its owner updated to
    //  our new version. This only affects versions with shared storage
    //  counters since placements_bucket_id is otherwise cleared in
    //  new_version_fixup().
    //
    //  This refresh is crucial since it will avoid the GC deleting the
    //  bucket prematurely.
    if (REP.DBObject/*REP.Bucket*/ bucket =
	internal_get_bucket_no_create("placements_bucket_id")) {
      bucket->set_fields( ([ "owner": this ]) );
    }
  }

  if (flush_cache)
    flush_all_versions_cache();

  if (sizeof (changed_shared_fields || ([]))) {
    process_shared_storage_pgvs(lambda(REP.PGVersion pgv)
    {
      if (pgv != this) {
	// Don't want to share data structures between PGVersions
	// since that may cause DBObject.internal_set_raw to leave
	// changes unnoticed.
	pgv->update_shared_storage_fields (copy_value (changed_shared_fields));
      }
    });
  }

  return this;
}

int(0..1) set_fields (mapping(string:mixed) fields,
		      void|REP.OnError on_value_error)
//! The fields "page_type" and "page_status" can be used to set
//! page_type_id and page_status_id through the symbolic names.
//!
//! user_name and user_fullname are set automatically from the
//! "user_id" field. If no user_id is specified for a new record, then
//! the authenticated user in the REP session is used. Setting user_id
//! to zero sets all three to NULL in the record.
//!
//! @note
//! "pages" and "text_flow_links" cannot be set through this function.
//! Use the @[REP.Page] object for pages, and @[set_text_flow_link]
//! for text flow links.
{
  ASSERT_DBOBJECT_WRITE_PERM;
  ASSERT_IF_DEBUG (zero_type (fields->text_flow_links));

  if (!fields->storage_counter && !rec->storage_counter)
    fields->storage_counter = REP.get_counter ("storage_counter")->get();
  if (!fields->date_created && !rec->date_created)
    fields->date_created = time();

  if (string page_type = m_delete (fields, "page_type"))
    fields->page_type_id = REP.DB.get_page_type_id (page_type);
  if (string page_status = m_delete (fields, "page_status"))
    fields->page_status_id = REP.DB.get_page_status_id (page_status);

  if (!fields->page_type_id && !rec->page_type_id)
    fields->page_type_id = REP.DB.get_page_type_id ("editor");
  if (!fields->page_status_id && !rec->page_status_id)
    fields->page_status_id = REP.DB.get_page_status_id ("inproduction");

  if (zero_type (fields->user_id) &&
      (zero_type (rec->user_id) || rec->user_id == Val.null)) {
    //  Only store new user ID if non-zero. User ID zero is still valid but
    //  internal and it triggers debug assertions if it's set from callbacks
    //  for existing PGVs.
    if (int new_user_id = REP.get_session()->user_id()) {
      //  Don't update user info for live PGVs
      int pgv_id = rec->page_version_id;
      int cur_pgv_id = _page_group_rec && _page_group_rec->cur_pgversion_id;
      bool is_live = pgv_id && (pgv_id <= cur_pgv_id);
      if (!is_live)
        fields->user_id = new_user_id;
    }
  }

  if (int user_id = fields->user_id) {
    AC.ModuleAC mac = tbl->print_db->my_mac();
    if (!mac->id_get_id (user_id)) {
      m_delete (fields, "user_id");
    } else {
      if (string handle = mac->id_get_handle (user_id))
	fields->user_handle = handle;
      if (string user_fullname = mac->id_get_name (user_id))
	fields->user_fullname = user_fullname;
    }
  }

  if (!zero_type (fields->comment) && !fields->comment)
    fields->comment = "";

  low_set_raw (fields);

  if (!zero_type(fields->preflight_results))
    invalidate_cached_ext_field("preflight_results_txt");

  return 1;
}

void set_preflight_results(mapping(string:mixed) res)
{
  //  Set and push update to clients subscribing to the page plan
  set_fields( ([ "preflight_results": res ]) );
  array(REP.PageSlot) pss = pages()->page_slot() - ({ 0 });
  if (sizeof(pss))
    REP.Notification.Client.edition_content_update(edition(), pss);
}

void invalidate_jump_preflight_results(void|bool skip_notification)
{
  //  Used when the actual IDS preflight data hasn't changed but the
  //  piggy-backed jumps data is stale.
  invalidate_cached_ext_field("preflight_results_txt");
  array(REP.PageSlot) pss = pages()->page_slot() - ({ 0 });
  if (sizeof(pss) && !skip_notification)
    REP.Notification.Client.edition_content_update(edition(), pss);
}

mixed get_ext (string field)
{
  switch (field) {
  case "markup_error":
    return get ("markup_error") || 0;

  case "page_status":
    return page_status();

  case "package_file_downloadable":
    return package_file_downloadable();

  case "preflight_results_txt":
    return describe_preflight_results() || Val.null;

  case "file_urls":
  case "file_urls_ondemand":
    multiset ext_file_types =
      (< REP.Storage.UNPROC_LAYOUT_FILE, REP.Storage.LAYOUT_FILE,
         REP.Storage.PACKAGE_FILE, REP.Storage.PDF_GROUP_PREVIEW,
         REP.Storage.PDF_GROUP_PREVIEW_LR >);

    mapping file_urls = ([]);
    REP.PrintDBModule print_db = REP.get_print_module();

    foreach (ext_file_types; REP.Storage.FileType file_type;) {
      if ((field == "file_urls_ondemand") || is_valid_file(file_type))
        file_urls[file_type] = print_db->get_pgv_url (this, 0, file_type);
    }

    return file_urls;
  case "permission":
    return access_perm();
  default:
    return ::get_ext (field);
  }
}


// Unversioned storage handling

mapping(string:mixed) get_rec_unver()
//! Returns the unversioned properties for this object.
{
  return page_group_rec() + ([]);
}

mixed get_unver (string field)
//! Returns the value of a field in the unversioned part of the page
//! group, i.e. in the page_groups record.
{
  ASSERT_IF_DEBUG (avail_unver_fields[field]);
  if (field == "is_edit_locked") check_edit_lock_timeout();

  return page_group_rec()[field];
}

void low_set_raw_unver (mapping(string:mixed) vals)
//! Sets several fields at once in the unversioned part of the page
//! group, i.e. in the page_groups record. Is destructive on @[vals].
//!
//! @note
//! Cannot be used to change the current page group version; call
//! @[REP.DB.make_pgv_current] instead.
{
  REP.CachedTable pg_tbl = REP.DB.page_groups_table();

  // NB: This might be called in "infant" PGVersion objects which
  // haven't been through ObjectTable.low_object_create yet. Therefore
  // we cannot use page_group_rec() right away here.

  ASSERT_IF_DEBUG (!vals->page_group_id || !rec->page_group_id);
  mapping(string:mixed) pg_rec;
  if (vals->page_group_id)
    pg_rec = pg_tbl->cached_get (vals->page_group_id);
  else if (rec->page_group_id)
    pg_rec = page_group_rec();

  ASSERT_IF_DEBUG (!pg_rec || !pg_rec[0]);
  ASSERT_IF_DEBUG (sizeof (vals & avail_unver_fields) == sizeof (vals));
  ASSERT_IF_DEBUG (!vals->edition_id || !pg_rec);
  ASSERT_IF_DEBUG (!vals->page_group_uuid || !pg_rec);
  ASSERT_IF_DEBUG (!vals->cur_pgversion_id);

  int was_deleted = pg_rec && pg_rec->is_deleted;

  if (!pg_rec) {
    int pg_id = REP.DB.page_groups_table()->cached_insert (vals);
    foreach (unver_col_defaults; string field; mixed value)
      vals[field] = value;

    // Earliest point when we have an id for the new page group, so we
    // can put pg_volatiles in the global mapping.
    tbl->print_db->pg_volatiles[pg_id] = pg_volatile;

    low_set_raw ((["page_group_id": pg_id]));
  }

  else {
    foreach (vals; string field; mixed value) {
      mixed pg_value = pg_rec[field];
      if (!zero_type (pg_value) && equal (value, pg_value))
	m_delete (vals, field);
    }

    if (sizeof (vals)) {
      vals->page_group_id = rec->page_group_id;
      REP.DB.page_groups_table()->cached_update (vals);
    }
  }

  if (!vals->is_deleted != !was_deleted) {
    if (this_program cur_pgv = current_pgv())
      // If the page group is deleted then all its records in the
      // pages table should be removed, and the assigned slots should
      // get placeholder replacements as appropriate.
      //
      // Conversely, if it gets undeleted again we assign it back to
      // its old slots, as far as they still exist. (This might be
      // somewhat disruptive, but is the right thing to do to make a
      // deletion reversible. If it is changed, then a new pgv with
      // unassigned pages must be constructed here.)
      cur_pgv->internal_update_pages (1);

    // FIXME: Is this the right place? The page change counter should
    // only need to be updated when a page slot assignment changes,
    // and that only happens when pages change in the current pgv, iow
    // when the pages property gets mirrored to the pages db table,
    // i.e. in internal_update_pages.
    edition()->bump_page_changed();
  }

  m_delete (vals, "page_group_id");
}

int(0..1) set_unver_fields (mapping(string:mixed) fields,
			    void|REP.OnError on_value_error)
{
  ASSERT_DBOBJECT_WRITE_PERM_FB (_page_group_rec &&
				 _page_group_rec->page_group_id,
				 REP.DB.edition_by_id (fields->edition_id ||
						       page_group_rec()->edition_id));
  return set_unver_fields_noperm(fields, on_value_error);
}

protected int(0..1) set_unver_fields_noperm (mapping(string:mixed) fields,
					     void|REP.OnError on_value_error)
//! Like @[set_fields], but for the unversioned part of the page
//! group.
//!
//! New records get an page_group_uuid automatically if none is
//! specified.
//!
//! edit_user_name and edit_user_fullname are set automatically from
//! the edit_user_id field. Setting edit_user_id to zero sets all
//! three to NULL in the record.
//!
//! Clearing is_edit_locked also clears the edit_user_* fields.
{
  if (!rec->page_group_id && !fields->page_group_uuid)
    fields->page_group_uuid = Standards.UUID.make_version4()->str();

  if (!zero_type (fields->is_edit_locked) && !fields->is_edit_locked) {
    fields->edit_user_id = Val.null;
    fields->edit_user_handle = Val.null;
    fields->edit_user_fullname = Val.null;
  }

  else {
    int user_id = fields->edit_user_id;
    if (!zero_type (user_id)) {
      AC.ModuleAC mac = tbl->print_db->my_mac();
      fields->edit_user_handle = (user_id && mac->id_get_handle (user_id)) ||
	Val.null;
      fields->edit_user_fullname = (user_id && mac->id_get_name (user_id)) ||
	Val.null;
      if (!user_id) fields->edit_user_id = Val.null;
    }
  }

  low_set_raw_unver (fields);
  return 1;
}

void internal_update_pages (int update_pages_table)
// Mirrors the pages property into the pages table, and also invalidates
// the cached page references in the associated page slots (which must
// be done when a new pgv is made current, even for page assignments
// that haven't changed).
{
  int pg_deleted = get_unver ("is_deleted");

  mapping(int:int(1..2)) invalidate_slots = ([]);
  // The value 2 means the slot is assigned a page, 1 that it got unassigned
  // from a page (in this page group). Might contain zero in the index.

  if (!pg_deleted)
    foreach (rec->pages || ([]);; mapping(string:mixed) page_rec)
      invalidate_slots[page_rec->page_slot_id] = 2;

  REP.DB.TableLock lock;

  multiset(REP.PGVersion) deleted_pgvs = (<>);

  if (update_pages_table) {
    ASSERT_IF_DEBUG (
      sizeof (pages()->get ("page_slot_id") - ({0})) ==
      sizeof (Array.uniq (pages()->get ("page_slot_id") - ({0}))));

    REP.CachedTable pages_tbl = REP.DB.pages_table();

    mapping(REP.Page:REP.PageSlot) unassigned_pages = ([]);
    // The pages from other page groups that got unassigned from the slots.

    lock = REP.DB.TableLock (REP.DB.table_locks_for_pgv_and_page_changes);

#ifndef ENABLE_SCRATCH_AREA
    // To be able to call unlocked_copy_page_slot.
    Thread.MutexKey ps_lock = REP.DB.page_slot_order_is_locked() ||
      REP.DB.page_slot_order_lock();
#endif

    // Note that REP.DB.swap_pages_in_slots changes the table directly
    // to avoid that the following code detects the change.
    mapping(int:mapping(string:mixed)) old_recs = ([]);
    foreach (pages_tbl->cached_select ("page_group_id = " +
				       rec->page_group_id);;
	     mapping(string:mixed) old_rec) {
      foreach (old_rec; string idx; mixed value) {
	// Normalize away null values to avoid bogus updates after
	// comparing old_rec with new_rec below. (See notes on
	// Val.null values in the docs for the @[rec] mapping in
	// Page.pike).
	if (value == Val.null)
	  m_delete (old_rec, idx);
      }
      old_recs[old_rec->page_id] = old_rec;
    }

    int page_group_id = rec->page_group_id;

    foreach ((!pg_deleted && rec->pages) || ([]);;
	     mapping(string:mixed) page_rec) {
      mapping(string:mixed) old_rec =
	m_delete (old_recs, page_rec->page_id);
      mapping(string:mixed) new_rec =
	page_rec + (["page_group_id": page_group_id]);

      if (!equal (old_rec, new_rec)) {
	if (!old_rec || old_rec->page_slot_id != new_rec->page_slot_id) {
	  if (old_rec && !invalidate_slots[old_rec->page_slot_id])
	    invalidate_slots[old_rec->page_slot_id] = 1;
	  if (REP.PageSlot new_slot = new_rec->page_slot_id &&
	      // Don't fuss if page_slot_id is invalid - it can happen if an
	      // old page group is undeleted and it refers to slots that no
	      // longer exist.
	      REP.DB.page_slot_by_id (new_rec->page_slot_id, REP.RETURN_ZERO))
	    if (REP.Page unassigned_page = new_slot->low_get_page())
	      // Pages in our own page group should have been handled by
	      // Page.low_set_raw.
	      if (unassigned_page->pgv()->get ("page_group_id") !=
		  rec->page_group_id)
		unassigned_pages[unassigned_page] = new_slot;
	}

	int id = pages_tbl->cached_replace (new_rec);
	ASSERT_IF_DEBUG (id == page_rec->page_id);
      }
    }

    if (sizeof (old_recs)) {
      pages_tbl->cached_remove_multi (indices (old_recs));
      foreach (old_recs;; mapping(string:mixed) old_rec)
	if (!invalidate_slots[old_rec->page_slot_id])
	  invalidate_slots[old_rec->page_slot_id] = 1;
    }

    // Handle the pages that were assigned to the slots before: If they
    // belonged to placeholders then we delete them now, otherwise
    // unassign the pages.
    mapping(this_program:mapping(REP.Page:mapping(string:mixed)))
      pgv_page_changes = ([]);
    int now = time();
    foreach (unassigned_pages; REP.Page page; REP.PageSlot old_slot) {
      if (!REP.DB.delete_placeholder_pgv (page->pgv())) {
#ifdef ENABLE_SCRATCH_AREA
	// Simply unassigning the pages is the right way but it
	// requires a working scratch area in the UI.
	pgv_page_changes[page->pgv()] += ([page: (["page_slot_id": Val.null])]);
#else
	// For now we have to create deleted page slots to assign the
	// pages to, or else they get inaccessible from the UI. :(
	// Note that we only ensure that pages keeps being assigned to
	// some slot once they get assigned to one, so it's still
	// possible to create pages without slot assignments.
        // We'll specify "after_slot" to make sure the placeholder
        // doesn't become a sibling, and clear any move lock to avoid a
	// potential error in the slot's field check.
        int old_pg_num = old_slot->get("calc_page_no");
	REP.PageSlot repl_slot =
	  REP.DB.unlocked_copy_page_slot (old_slot,
                                          (["is_deleted": 1,
                                            "delete_at": now,
                                            "after_slot": old_slot,
					    "is_move_locked": 0]));

        //  Show old page number in trashcan view. We need to trick the slot
        //  by bypassing set_fields() which otherwise blocks this change.
        if (old_pg_num) {
          repl_slot->
            internal_update_page_fields( ([ "calc_page_no": old_pg_num ]) );
        }

	pgv_page_changes[page->pgv()] += ([page: (["page_slot": repl_slot])]);
#endif
      }
      deleted_pgvs[page->pgv()] = 1;
    }
    foreach (pgv_page_changes;
	     this_program pgv;
	     mapping(REP.Page:mapping(string:mixed)) page_changes)
      REP.DB.unlocked_set_in_current_pgv (pgv, 0, page_changes);
  }

  // Add story apps from deleted pgvs.
  foreach (deleted_pgvs; REP.PGVersion del_pgv;) {
    // REP.DB.copy_stories_between_page_groups (and called
    // subfunctions) have been notorious for throwing errors. We'll
    // catch and report such errors since our PGVersion (and possibly
    // other things) might end up in an inconsistent state
    // otherwise. Story copying is not absolutely essential for the
    // consistency of the operation either.
    if (mixed err = catch {
	// Provide the "reset_placements" flag since we're discarding
	// the old PGVersion - i.e. the copied stories aren't placed
	// on the new pgv per se.
	REP.DB.copy_stories_between_page_groups (del_pgv, this, 0, 0, 1);
      })
      werror ("Error in %O->internal_update_pages():\n%s\n",
	      this, describe_backtrace (err));
  }

  if (sizeof (invalidate_slots)) {
    REP.ObjectTable page_slots_tbl = REP.DB.page_slots_table();
    foreach (invalidate_slots; int page_slot_id; int op) {
      REP.PageSlot slot = page_slots_tbl->get_from_cache (page_slot_id);
      if (slot)
	slot->invalidate_pages();
      if (op == 1 && page_slot_id)
	// The slot got no page at all after being unassigned, so we need to
	// ensure it gets a placeholder page group.
	(slot || REP.DB.page_slot_by_id (page_slot_id))->page();
    }
  }

  edition()->invalidate_text_chains();
}

void new_version_fixup (int flush_metadata)
// Called after cloning the fields from the previous version to this
// new one (see REP.DB.low_make_infant_pgv). The task is to
// update/reset various fields that cannot simply be copied. The
// exception is storage_counter which has already been fixed by the
// caller.
{
  if (flush_metadata) {
    rec->markup_error = 0;
    rec->preflight_results = 0;
    rec->file_md = 0;
    rec->indesign_version = 0;
    rec->placements_bucket_id = 0;

    foreach (rec->pages;; mapping(string:mixed) page_rec) {
      m_delete (page_rec, "split_layout_file");
      m_delete (page_rec, "layout_info");
      m_delete (page_rec, "page_items_by_id");
    }
  }

  if (!rec->user_storage_counter && !zero_type (rec->user_storage_counter))
    m_delete (rec, "user_storage_counter");
}

protected void check_edit_lock_timeout()
{
  int edit_lock_expiry = get_unver ("edit_lock_expiry");

  if (edit_lock_expiry && edit_lock_expiry < time()) {
    //  Skip permission check since this can happen as a side-effect of a
    //  read-only request.
    set_unver_fields_noperm( ([ "is_edit_locked" : 0,
				"edit_lock_expiry" : 0 ]) );
  }
}

int(0..1) try_edit_lock (void|int(0..1) same_user_can_relock,
			 void|int timeout,
			 void|int(0..1) flag_server_busy,
			 void|REP.OnError on_lock_failed)
{
  REP.DB.TableLock lock =
    REP.DB.TableLock ( ([ "page_groups" : REP.DB.WRITE_LOCK,
			  // The following are needed by access_perm()
			  "editions": REP.DB.READ_LOCK,
			  "publications": REP.DB.READ_LOCK,
		       ]) );

  int user_id = REP.get_session()->user_id();
  int is_edit_locked = get_unver ("is_edit_locked");
  int locked_user_id = get_unver ("edit_user_id");

  if (is_edit_locked) {
    if (!same_user_can_relock || locked_user_id != user_id) {
      destruct (lock);
      return REP.raise_err (on_lock_failed, "Page edit lock already locked.\n");
    }
  }

  mapping(string:mixed) changed_fields =
    ([ "is_edit_locked"   : flag_server_busy ? 2 : 1,
       "edit_user_id"     : user_id,
       "edit_lock_expiry" : timeout && (time() + timeout) ]);

  set_unver_fields (changed_fields);

  destruct (lock);
  return 1;
}

int (0..1) user_has_permanent_edit_lock()
{
  int user_id = REP.get_session()->user_id();
  int is_edit_locked = get_unver("is_edit_locked");
  int locked_user_id = get_unver("edit_user_id") || Val.null;

  // set_unver_fields sets edit_user_id to NULL if it's zero, so we
  // need to match that in this comparison.
  if (is_edit_locked && (locked_user_id == (user_id || Val.null))) {
    int edit_lock_expiry = get_unver ("edit_lock_expiry");
    return edit_lock_expiry == 0;
  }
  return 0;
}

void release_edit_lock (void|int only_if_same_user_id)
{
  REP.DB.TableLock lock =
    REP.DB.TableLock ( ([ "page_groups" : REP.DB.WRITE_LOCK,
			  // The following are needed by access_perm()
			  "editions": REP.DB.READ_LOCK,
			  "publications": REP.DB.READ_LOCK,
                          "page_status": REP.DB.READ_LOCK,
		       ]) );
  int user_id = REP.get_session()->user_id();

  int locked_user_id = get_unver ("edit_user_id");

  // set_unver_fields sets edit_user_id to NULL if it's zero, so we
  // need to match that in this comparison.
  if (only_if_same_user_id && locked_user_id != (user_id || Val.null))
    return;

  set_unver_fields ( ([ "is_edit_locked" : 0 ]) );
  destruct (lock);
}

void set_edit_lock_timeout (int timeout)
{
  set_unver_fields (([ "edit_lock_expiry": time() + timeout ]));
}

__deprecated__ void release_edit_lock_by_crc32 (string crc32)
{
  release_edit_lock_by_file_hash (crc32);
}

void release_edit_lock_by_file_hash (string hash)
{
  add_pending_op (hash, PendingOperation (PENDING_RELEASE_EDIT_LOCK));
  perform_pending_operations();
}

__deprecated__ void set_page_status_by_crc32 (string crc32, string status)
{
  set_page_status_by_file_hash (crc32, status);
}

void set_page_status_by_file_hash (string hash, string status)
{
  add_pending_op (hash, PendingOperation (PENDING_SET_PAGE_STATUS, status));
  perform_pending_operations();
}

void add_make_current_cb(function cb, mixed ... args)
{
  //  Use the magic "*" checksum since we're not interested in a specific
  //  version as such, only when the page is made current.
  array(mixed) call_ctx = ({ cb }) + args;
  add_pending_op("*",
                 PendingOperation(PENDING_CALL_CB_WHEN_MADE_CURRENT, call_ctx));
  perform_pending_operations();
}

constant PENDING_RELEASE_EDIT_LOCK = "release-edit-lock";
constant PENDING_SET_PAGE_STATUS = "set-page-status";
constant PENDING_CALL_CB_WHEN_MADE_CURRENT = "call-cb-when-made-current";

typedef string PendingOp;

class PendingOperation(PendingOp operation, void|mixed value)
{
  int(0..1) execute (REP.PGVersion pgv)
  {
    REP.PGVersion cur_pgv = pgv->current_pgv();
    int pgv_storage_counter = pgv->get ("user_storage_counter") ||
      pgv->get ("storage_counter");
    int cur_storage_counter = cur_pgv->get ("user_storage_counter") ||
      cur_pgv->get ("storage_counter");

    switch (operation) {
    case PENDING_RELEASE_EDIT_LOCK:
      // Unlock requests for old versions are ignored, but we need to
      // take user_storage_counter into account to avoid races with
      // automatic updating code that runs automatically on page
      // upload (e.g. delayed ad rendering requests.)

      // Wait until the pgv has been made current to avoid
      // races for operations that trig on edit lock release.
      if (pgv_storage_counter > cur_storage_counter)
        // Live version newer than current, return 0 to try again later.
        return 0;

      if (pgv_storage_counter < cur_storage_counter)
        // Old version, ignore and return success.
        return 1;

      pgv->set_unver_fields ( ([ "is_edit_locked": 0 ]) );
      break;

    case PENDING_SET_PAGE_STATUS:
      //  Only set status if new value differs from current setting
      if (pgv->id() > pgv->get_unver ("cur_pgversion_id")) {
        if (pgv->page_status() != value)
          pgv->set_fields ( ([ "page_status": value ]) );
      } else {
        if (pgv->current_pgv()->page_status() != value)
          REP.DB.set_in_current_pgv (pgv, ([ "page_status": value ]));
      }
      break;

    case PENDING_CALL_CB_WHEN_MADE_CURRENT:
      if ((pgv != pgv->current_pgv()) &&
	  (pgv->id() > pgv->current_pgv()->id())) {
	// NB: REP.DB.unlocked_make_current() enforces a
	//     monotonically increasing cur_pgversion_id,
	//     so if current_pgv has passed us by, we
	//     pretend that the pgv has been made current.
	//
        //  Not yet current. Return 0 to try again later.
        return 0;
      }

      array(REP.PGVersion) pgv_history = all_versions(1, 0);
      if (!sizeof(pgv_history) || (pgv != pgv_history[0]))
        //  This is an old version. Return 0 to try again for another version.
        return 0;

      if (array call_ctx = value) {
        function cb = call_ctx[0];
        array(mixed) args = call_ctx[1..];
        if (mixed err = catch {
            cb(@args);
          })
          werror("Error in PendingOperation->execute():\n%s\n",
                 describe_backtrace(err));
      }
      break;
    }

    return 1;
  }
}

// Checksum remaps. Maps real checksums for LAYOUT_FILEs to the checksum
// of an upconverted document, if any. These are stored in memory only.
protected void add_checksum_remap(string from, string to)
{
  if (!pg_volatile->checksum_remaps) {
    pg_volatile->checksum_remaps = ([]);
  }
  pg_volatile->checksum_remaps[from] = to;
}

// The @[from] argument is typically any of the checksum forms (CRC, MD5, SHA)
// of a LAYOUT_FILE. If that document has been sent in upconverted form to a
// client, the hash string of that upconverted document will be returned.
// Remaps are stored in memory only.
protected string get_checksum_remap(string from)
{
  mapping(string:string) remaps = pg_volatile->checksum_remaps || ([]);
  return remaps[from];
}

protected void process_ops_for_checksum(string checksum, REP.PGVersion pgv)
{
  if (array(PendingOperation) ops =
      m_delete (pending_ops_mapping(), checksum)) {
    array(PendingOperation) restore_ops = ({});
    foreach (ops, PendingOperation op) {
      if (!op->execute (pgv)) {
        restore_ops += ({ op });
      }
    }

    if (sizeof (restore_ops)) {
      pending_ops_mapping()[checksum] =
        (pending_ops_mapping()[checksum] || ({})) + restore_ops;
    }
  }
}

protected void perform_pending_operations
(void|REP.PGVersion pgv,
 void|array(REP.Storage.FileType) file_types)
{
  if (!sizeof (pending_ops_mapping())) return;

  if (!file_types) file_types = ({ REP.Storage.UNPROC_LAYOUT_FILE,
				   REP.Storage.LAYOUT_FILE });

  array(int|REP.PGVersion) check_pgvs;
  if (pgv) {
    check_pgvs = ({ pgv });
  } else {
    check_pgvs = tbl->select1 ("page_version_id",
			       // WHERE
			       "page_group_id = " +
			       rec->page_group_id + " "
			       "  AND is_deleted = 0", 0,
			       " ORDER BY page_version_id DESC");
  }

  foreach (check_pgvs, mixed pgv) {
    if (intp (pgv)) pgv = REP.DB.pgv_by_id (pgv);

    foreach (file_types, REP.Storage.FileType file_type) {
      foreach (pgv->get_page_file_checksums (file_type) || ([]);
	       string idx;
	       string checksum) {
        process_ops_for_checksum(checksum, pgv);

        // pending_ops_mapping may contain checksums (received from clients) for
        // upconverted documents. We need to check if a source LAYOUT_FILE has
        // been upconverted by looking for its checksum here.
        if (string remapped = get_checksum_remap(checksum)) {
          // Process ops for the upconverted document.
          process_ops_for_checksum(remapped, pgv);
        }
      }
    }

    //  Also process the magic "*" checksum which matches anything
    process_ops_for_checksum("*", pgv);

    //  If no checksums remain to be processed we can stop here and avoid
    //  instantiating older PGVs. We could optimize this further for cases
    //  where the pending request is known to only cover latest/current (or
    //  close) and then limit the number of objects we scan.
    if (!sizeof(pending_ops_mapping()))
      return;
  }
}

protected void add_pending_op (string checksum, PendingOperation op)
{
  pending_ops_mapping()[checksum] =
    (pending_ops_mapping()[checksum] || ({})) + ({ op });
}

protected mapping pending_ops_mapping()
{
  if (!pg_volatile->pending_ops_mapping)
    pg_volatile->pending_ops_mapping = ([]);
  return pg_volatile->pending_ops_mapping;
}

Thread.Mutex deferred_ops_mutex = Thread.Mutex();

void add_deferred_page_update (REP.IDS.PageUpdate update)
{
  Thread.MutexKey lock = deferred_ops_mutex->lock();

  array(REP.IDS.PageUpdate) deferred_page_updates = ({ });
  if (string encoded = get_unver("deferred_page_updates")) {
    deferred_page_updates = decode_value (encoded);

    ASSERT_IF_DEBUG(arrayp(deferred_page_updates));
    ASSERT_IF_DEBUG(sizeof(deferred_page_updates));
  }

  //  Add new operation to queue. When doing so we also remove any existing
  //  operations that declare the same coalescing key which optimizes away
  //  redundant operations (e.g. repeated shadow copying).
  if (string coalesce_key = update->coalesce_key) {
    deferred_page_updates =
      filter(deferred_page_updates, lambda(REP.IDS.PageUpdate pu) {
          return pu->coalesce_key != coalesce_key;
        });
  }
  deferred_page_updates += ({ update });

  set_unver_fields (([ "deferred_page_updates":
                       encode_value (deferred_page_updates) ]));
}

Concurrent.Future/*REP.PGVersion*/ execute_deferred_updates()
{
  return low_execute_deferred_updates(0);
}

protected Concurrent.Future/*REP.PGVersion*/
low_execute_deferred_updates(Concurrent.Promise batch_promise)
{
  //  We can only execute one deferred update at a time since we need to
  //  wait for the IDS completion callbacks to run (so new PGVs are made
  //  current). If there are multiple updates queued we'll put the rest
  //  back for later consideration without releasing the edit lock in
  //  between.
  Thread.MutexKey lock = deferred_ops_mutex->lock();

  array(REP.IDS.PageUpdate) deferred_page_updates;
  if (string encoded = get_unver("deferred_page_updates")) {
    deferred_page_updates = decode_value (encoded);
  }

  if (!deferred_page_updates) {
    if (batch_promise)
      batch_promise->success(this);
    return 0;
  }

  if (!try_edit_lock(0, 0, 1, REP.RETURN_ZERO)) {
    // We'll be invoked again when the lock is released.
    return 0;
  }

  REP.IDS.PageUpdate deferred_pu = deferred_page_updates[0];
  deferred_page_updates = deferred_page_updates[1..];
  set_unver_fields (([ "deferred_page_updates":
                       sizeof(deferred_page_updates) ?
                       encode_value(deferred_page_updates) : UNDEFINED ]));

  //  Avoid garbage in promise context
  deferred_page_updates = 0;

  lock = 0;

  if (!batch_promise)
    batch_promise = Concurrent.Promise();
  Concurrent.Future future = deferred_pu->perform_deferred_updates();

  future
    ->recover(lambda(mixed err) {
                werror ("%O.execute_deferred_updates: Error: %s",
                        this, describe_error(err));
                return 1;
              })
    ->on_success(REP.cb_wrapper(lambda() {
                                  release_edit_lock();
                                  low_execute_deferred_updates(batch_promise);
                                }));

  return batch_promise->future();
}

// REP.Page handling

protected mapping(int(1..):REP.Page) pages_by_page_id;
// This is used to keep track of the REP.Page instances - only cleared
// on deferred_purge (see delete()).

#define LOOKUP_KEY(page_id) (string)rec->page_version_id + ":" + (string)page_id

protected void add_in_use_pages (mapping(int(1..):REP.Page) _pages_by_page_id)
{
  mapping(string:REP.Page) in_use_pages = REP.get_print_module()->in_use_pages;
  foreach (_pages_by_page_id; int page_id; REP.Page page) {
    in_use_pages[LOOKUP_KEY(page_id)] = page;
  }
}

protected mapping(int(1..):REP.Page) get_pages_by_page_id()
{
  if (!pages_by_page_id) {
    if (mapping(int(1..):mapping(string:mixed)) enc_pages = rec->pages) {
      mapping(string:REP.Page) in_use_pages =
	REP.get_print_module()->in_use_pages;
      mapping(int(1..):REP.Page) new_pages_by_page_id = ([]);

      foreach (enc_pages; int page_id; mapping(string:mixed) page_rec) {
	// If there's already a REP.Page object instantiated
	// representing this page, make sure we avoid duplicates. This
	// can happen when PGVersion objects are expired from the
	// cache and then reinstantiated, while their referenced
	// REP.Page objects are referenced elsewhere.
	if (REP.Page existing_page = in_use_pages[LOOKUP_KEY(page_id)]) {
	  new_pages_by_page_id[page_id] = existing_page;
	} else {
	  new_pages_by_page_id[page_id] = REP.Page (page_rec, this);
	}
      }

      // Take care of races.
      if (!pages_by_page_id) {
	pages_by_page_id = new_pages_by_page_id;
	if (rec->page_version_id) {
	  // We only add to the global pages mapping if we aren't an
	  // infant. Also performed in commit() when infants get a
	  // record id.
	  add_in_use_pages (pages_by_page_id);
	}
      }
    } else {
      pages_by_page_id = ([]);
    }
  }
  return pages_by_page_id;
}

protected mapping(string:REP.Page) pages_by_ext_page_id;
// Only contains the pages that actually have ext_page_id's.

protected array(REP.Page) pages_in_doc_order;

REP.Page page_by_id (int(1..) page_id, void|REP.OnError on_not_found)
//! Returns the page specified by its record id.
{
  if (REP.Page page = (pages_by_page_id || get_pages_by_page_id())[page_id])
    return page;
  return REP.raise_err (on_not_found, "Page id %O not found.\n", page_id);
}

REP.Page page_by_ext_id (string ext_page_id, void|REP.OnError on_not_found)
//! Returns the page specified by its ext_page_id.
{
  mapping(string:REP.Page) page_map = pages_by_ext_page_id;
  if (!page_map) {
    page_map = ([]);
    foreach (get_pages_by_page_id();; REP.Page page)
      if (string ext_id = page->get ("ext_page_id"))
	page_map[ext_id] = page;
    pages_by_ext_page_id = page_map;
  }

  if (REP.Page page = page_map[ext_page_id])
    return page;
  return REP.raise_err (on_not_found, "No page with external id %O.\n",
			ext_page_id);
}

array(REP.Page) pages()
//! Returns the pages in the order they occur in the layout file (or,
//! specifically, ordered according to the doc_order field).
//!
//! @seealso
//! @[REP.DB.sort_pages_by_page_slot_order], @[REP.DB.sort_pages_by_page_order]
{
  array(REP.Page) pgs = pages_in_doc_order;
  if (!pgs)
    pages_in_doc_order = pgs =
      REP.DB.sort_pages_by_doc_order (values (get_pages_by_page_id()));
  return pgs;
}

void internal_invalidate_page_caches()
{
  pages_in_doc_order = pages_by_ext_page_id = 0;
}

void internal_changed_page()
// Called when anything inside the rec->pages property has changed.
{
  dirty_fields->pages = 1;
  if (!auto_commit_disabled_count && id())
    commit();
}

int(0..1) add_pages (array(mapping(string:mixed)) new_pages,
		     void|REP.OnError on_value_error)
//! Adds new pages to the page group. Unlike @[set_fields] etc, this
//! is allowed after the page group version is made current.
//!
//! The mappings in @[new_pages] contain the fields to set through
//! @[REP.Page.set_fields]. @[on_value_error] is passed on to that
//! function.
//!
//! The pages are appended to the doc_order by default, i.e. mappings
//! that lack the "doc_order" field receives the next available
//! doc_order, in the order they occur in @[new_pages].
{
  int status = 1;

  DisableAutoCommit dac = DisableAutoCommit();

  if (!rec->pages) rec->pages = ([]);
  if (!pages_by_page_id) get_pages_by_page_id();

  int last_doc_order = 0;
  foreach (pages_by_page_id;; REP.Page page) {
    int doc_order = page->get ("doc_order");
    if (doc_order > last_doc_order)
      last_doc_order = doc_order;
  }

  array(mapping(string:mixed)) added_page_recs = ({});

  foreach (new_pages, mapping(string:mixed) new_page) {
    // Normally the caller should never give page_id explicitly, but
    // it can be useful in corner cases like reconstructing old page
    // versions during migration. page_id is also provided when we're
    // called from update_shared_storage_fields.
    int(1..) new_id = (m_delete (new_page, "page_id") ||
		       REP.get_counter ("page_counter")->get());
    mapping(string:mixed) page_rec = rec->pages[new_id] =
      (["page_id": new_id]) + REP.Page.col_defaults;
    REP.Page p = REP.Page (page_rec, this);

    if (zero_type (new_page->doc_order))
      new_page->doc_order = ++last_doc_order;
    else if (new_page->doc_order > last_doc_order)
      last_doc_order = new_page->doc_order;

    if (!p->set_fields (new_page, on_value_error))
      status = 0;
    else {
      added_page_recs += ({page_rec});
      pages_by_page_id[new_id] = p;
    }
  }

  if (sizeof (added_page_recs)) {
    internal_invalidate_page_caches();
    internal_changed_page();
  }

  destruct (dac);

  return status;
}

void remove_last_pages (int(0..) count)
//! Removes the last @[count] pages according to the document order.
//! Like @[set_fields] etc, this is only allowed before the page group
//! version is made current.
{
  ASSERT_IF_DEBUG (!is_live);
  ASSERT_IF_DEBUG (count >= 0);
  ASSERT_IF_DEBUG (count <= sizeof (get_pages_by_page_id()));

  remove_pages (pages()[<count - 1..]);
}

void remove_pages (array(REP.Page) remove_pages)
{
  int changed = 0;

  foreach (remove_pages, REP.Page page) {
    ASSERT_IF_DEBUG (page->pgv() == this);
    int(1..) page_id = page->get ("page_id");
    if (rec->pages && m_delete (rec->pages, page_id)) {
      changed = 1;
      if (pages_by_page_id)
	m_delete (pages_by_page_id, page_id);
    }
  }

  if (changed) {
    internal_invalidate_page_caches();
    internal_changed_page();
  }
}

void delete(int(0..1)|void purge)
//! Mark this version as deleted.
//!
//! @param purge
//!   Purge the version and its associated files
//!   as soon as there are no references left to
//!   the version (or its files).
//! @seealso
//!   @[undelete()], @[deferred_purge]
{
  if (!rec->is_deleted) {
    low_set_raw(([ "is_deleted":1 ]));
  }

  // I've disabled immediate object purge since it might lead to
  // inconsistencies in e.g. Bucket owner references. Objects and
  // their references/referencees are cleaned up "atomically" by the
  // GC instead. /Marty
  /*
  if (purge) {
    // Set the deferred_purge flag and call internal_set_page_version
    // in our REP.Page instances, which will make them keep a
    // reference to this object. That way this object won't be
    // released prematurely, i.e. we'll avoid Page objects becoming
    // orphaned.
    deferred_purge = 1;
    foreach (get_pages_by_page_id(); int page_id; REP.Page page) {
      page->internal_set_page_version (this);
    }

    // Now, we want to get rid of references to the Page objects from
    // this object to avoid circular structures. In rare cases
    // something could make page lookups in this object again, which
    // will lead to new circularities, but we'll just leave that
    // special case to the periodic memory GC.
    pages_by_page_id = 0;
    internal_invalidate_page_caches();

    // Make sure the cached pg_storage_path is set here, since a full
    // lookup will fail in destroy() due to missing REPSession etc.
    get_storage_path();

    // We can finally expire this object from the cache.
    tbl->expire (this);
  }
  */
}

void undelete()
//! Remove the deletion marker for a version.
//!
//! This function also clears the @[deferred_purge] marker.
//! @seealso
//!   @[delete()], @[deferred_purge]
{
  if (rec->is_deleted) {
    low_set_raw(([ "is_deleted":0 ]));
  }
}

REP.PageSlot first_page_slot()
//! Returns the first slot - in page order - of those bound to any
//! page. May return zero if the page group is entirely on the scratch
//! area.
{
  REP.PageSlot res;
  foreach (pages(), REP.Page page)
    if (REP.PageSlot slot = page->page_slot())
      if (!res || slot < res)
	res = slot;
  return res;
}


// Story appearance handling

void invalidate_appearances(void|int(0..1) skip_story_app_invalidation)
{
  m_delete (pg_volatile, "stories_cache");
  m_delete (pg_volatile, "all_stories_cache");
  m_delete (pg_volatile, "story_ids_by_doc_ids");
  m_delete (pg_volatile, "sa_parts_cache");

  //  Caller may opt to skip this invalidation if it takes care of it
  //  separately. That is typically the case when appearances for a single
  //  story changes.
  if (!skip_story_app_invalidation) {
    foreach (stories (0, 1), REP.Story story) {
      story->invalidate_appearances();
    }
  }
}

mapping(int:mapping(string:mixed)) story_app_parts()
//! Returns the story_app_parts record mappings for this page group.
//!
//! @returns
//! The returned mapping maps the sa_part_ids of the story_app_parts
//! records to their record mappings. The record mappings also contain
//! a field "story_id" coming from the joined-in story_apps table.
//! Don't be destructive on any of the returned mappings.
//!
//! @note
//! The result contains appearances of all associated stories
//! regardless of the stories' home publication. It's not even
//! filtered according to permissions.
//!
{
  if (!rec->page_group_id)
    // Don't populate the cache for infants.
    return ([]);

  mapping(int:mapping(string:mixed)) sa_parts = pg_volatile->sa_parts_cache;
  if (!sa_parts) {
    sa_parts = ([]);
    // Ought to use the cached records, if we could find a way to add
    // story_id efficiently.
    foreach (REP.DB.story_app_parts_table()->select (
	       "page_group_id = " + rec->page_group_id,
	       0, ({"story_apps.story_id AS story_id"}),
	       "JOIN story_apps USING (story_app_id)");;
	     mapping(string:mixed) sa_part)
      sa_parts[sa_part->sa_part_id] = sa_part;
    pg_volatile->sa_parts_cache = sa_parts;
  }

  return sa_parts;
}

array(REP.Story) stories (void|int deleted, void|int all_publs,
			  void|int only_placed)
//! Returns the stories that appears in the current version of this
//! page group.
//!
//! @param deleted
//! Returns deleted stories if positive, non-deleted if zero, and both
//! variants if negative.
//!
//! @param all_publs
//! By default only stories in the same publication as this page group
//! are returned, but if this flag is set then all stories in all
//! publications are returned (subject to the callers publication
//! permissions).
//!
//! @param only_placed
//!   Specifies to only return stories that are placed on this pgv.
//!
//! @returns
//! Returns @[REP.Story] objects. Don't be destructive on the returned
//! array.
{
  if (!rec->page_group_id)
    // Don't populate the cache for infants.
    return ({});

  array(REP.Publication) publs;
  if (all_publs) {
    publs = REP.AC.readable_publ (1);
    all_publs = publs ? 1 : 2;
  }

  // Use the cache only for the default case when all_publs isn't set.
  array(int) story_ids = (all_publs == 2 ? pg_volatile->all_stories_cache :
			  !all_publs && pg_volatile->stories_cache);
  array(REP.Story) res = story_ids && map (story_ids, REP.DB.story_by_id);

  if (!res) {
    string table_refs = ("JOIN story_apps USING (story_id) "
			 "JOIN story_app_parts USING (story_app_id)");
    string where = "story_app_parts.page_group_id = " + rec->page_group_id;

    if (all_publs) {
      if (publs) {
	where += " AND stories.publication_id IN (" +
	  ((array(string)) publs->id() * ",") + ")";
      }
    }
    else
      where += " AND stories.publication_id = " +
	edition()->publication()->id();

    res = REP.DB.stories_table()->object_select (where, table_refs);

    if (all_publs == 2) pg_volatile->all_stories_cache = res->id();
    else if (!all_publs) pg_volatile->stories_cache = res->id();
  }

  return filter (res,
		 lambda (REP.Story story)
		 {
		   if (only_placed &&
		       !has_value (story->placed_on_pgvs(), this))
		     return 0;

		   int is_deleted = story->get ("delete_at");
		   if (deleted > 0 && !is_deleted)
		     return 0;
		   if (deleted == 0 && is_deleted)
		     return 0;

		   return 1;
		 });
}

mapping(int:REP.Story) stories_by_doc_ids (void|int deleted)
//! Returns the stories that have story_doc_id in their appearances,
//! i.e. those that are placed on the page.
//!
//! @param deleted
//! Returns deleted stories if positive, non-deleted if zero, and both
//! variants if negative.
{
  if (!rec->page_group_id)
    // Don't populate the cache for infants.
    return ([]);

  mapping(int:REP.Story) res;
  if (mapping(int:int) story_ids_by_doc_ids =
      pg_volatile->story_ids_by_doc_ids) {
    res = map (story_ids_by_doc_ids, REP.DB.story_by_id);
  } else {
    array(mapping(string:int)) db_res = REP.get_db()->typed_query (
      "SELECT story_apps.story_id AS story_id, story_doc_id "
      "FROM story_app_parts JOIN story_apps USING (story_app_id) "
      "WHERE story_app_parts.page_group_id = " + rec->page_group_id);
    array(REP.Story) stories =
      REP.DB.stories_table()->object_get_multi (column (db_res, "story_id"));

    res = ([]);
    foreach (db_res; int i; mapping(string:int) entry)
      if (entry->story_doc_id != Val.null)
	if (REP.Story story = stories[i])
	  res[entry->story_doc_id] = story;
    pg_volatile->story_ids_by_doc_ids = map (res,
					     lambda (REP.Story s)
					     { return s->id(); });
  }

  return (deleted > 0 ? filter (res, "get", "delete_at") :
	  !deleted ? filter (res, lambda (REP.Story story) {
				    return !story->get ("delete_at");
				  }) :
	  res);
}


// File storage handling

// Returns a mapping that is shared between all versions having the same
// storage counter.
private mapping storage_counter_mapping()
{
  if (!pg_volatile->storage_counter_mapping)
    pg_volatile->storage_counter_mapping = ([]);
  if (!pg_volatile->storage_counter_mapping[rec->storage_counter])
    pg_volatile->storage_counter_mapping[rec->storage_counter] = ([]);

  return pg_volatile->storage_counter_mapping[rec->storage_counter];
}

//! Adds a callback that will be executed when the current LAYOUT_FILE
//! (page preview) processing operation is finished (regardless of
//! success or failure). If no operation is in progress, the callback
//! will be executed immediately.
void add_ids_proc_finished_cb (function cb, void|mixed ... args)
{
  mapping scm = storage_counter_mapping();
  if (scm->ids_proc_finished_cbs) {
    scm->ids_proc_finished_cbs += ({ ({ cb }) + (args || ({})) });
  } else {
    cb (@args);
  }
}

protected void run_ids_proc_finished_cbs()
{
  if (array(array) cbs = storage_counter_mapping()->ids_proc_finished_cbs) {
    storage_counter_mapping()->ids_proc_finished_cbs = 0;
    foreach (cbs, array cb_entry) {
      array args = sizeof (cb_entry) > 1 ? cb_entry[1..] : ({});
      cb_entry[0] (@args);
    }
  }
}

void processing_attempt_started()
{
  storage_counter_mapping()->latest_processing_attempt = -1;
  storage_counter_mapping()->ids_proc_finished_cbs = ({});
}

void processing_attempt_successful()
{
  storage_counter_mapping()->latest_processing_attempt = time(1);
  run_ids_proc_finished_cbs();
}

void processing_attempt_failed()
{
  mapping scm = storage_counter_mapping();
  if (!scm->processing_attempt_delay)
    scm->processing_attempt_delay = 2;
  scm->processing_attempt_delay *= 2;
  scm->latest_processing_attempt = time(1);
  run_ids_proc_finished_cbs();

  //  This may be unforgiving but we don't know which files the cancelled
  //  request was about to generate so we don't dare to leave any promises
  //  hanging around indefinitely.
  cancel_all_waiting_promises();
}

protected int should_retry_processing()
{
  mapping scm = storage_counter_mapping();
  return scm->latest_processing_attempt != -1 &&
    (!scm->latest_processing_attempt ||
     time(1) > (scm->latest_processing_attempt +
		scm->processing_attempt_delay)) &&
    get_existing_page_filepath (REP.Storage.UNPROC_LAYOUT_FILE) && 1;
}

void process_ids (void|int(0..1) is_retry)
{
  if (should_retry_processing()) {
    processing_attempt_started();
    REP.IDS.process_unproc_pgv (this, is_retry);
  }
}

import REP.Storage;		// Plenty of REP.Storage.xxx below..

protected string pg_storage_path;

// Simple mapping from DB path to Stdio.Stat object for files in this
// page group. Note: shared by multiple PGVersion objects through
// pg_volatile. Initialized when needed.
protected mapping(string:Stdio.Stat) page_file_statcache;

void move_pg_storage_path (string old_path)
{
  pg_storage_path = 0;
  string new_path = get_pg_storage_path();
  if (has_suffix (new_path, "/")) new_path = new_path[..<1];
  if (has_suffix (old_path, "/")) old_path = old_path[..<1];
  if (stat_db_file (old_path))
    move_db_file (new_path, file_db_path() + old_path);
}

string get_pg_storage_path()
//! Similar to @[get_storage_path], but returns the path common to the
//! page group, i.e. one level up.
{
  return pg_storage_path ||
    (pg_storage_path =
     REP.Storage.pg_storage_path (edition(), get_unver ("page_group_uuid")));
}

string get_storage_path()
//! Returns the page version storage path (which might not exist on
//! disk). It is relative to the DB root and always ends with a "/".
//! Zero is never returned.
//!
//! @note
//! The returned path is suitable for @[REP.Storage.save_db_file].
//! That function also takes care of creating the directory if it
//! doesn't exist already.
{
  ASSERT_IF_DEBUG (rec->storage_counter);
  return (pg_storage_path || get_pg_storage_path()) +
    rec->storage_counter + "/";
}

string get_ext_tfl_field_path (string uuid, string field, int id)
// Returns the path to an externally stored text flow link field,
// based on a specific storage counter.
{
  return get_pg_storage_path() + id + "/" +
    sprintf (REP.Storage.filenames_subspec_by_type[field], uuid);
}

array(string) get_page_files (void|REP.Storage.FileType file_type,
			      void|int pathnames)
//! Returns a list of all @[file_type] files for this page version.
//! Temporary files, i.e. files ending with "~", are filtered. An
//! array is always returned.
//!
//! @param file_type
//! The requested file type. All files matching any file type are
//! returned if it's zero, and in that case the list is sorted in
//! reverse dependency order suitable for notification (see the class
//! doc for this class and @[REP.Storage.reverse_dependency_order]).
//!
//! @param pathnames
//! If nonzero then all entries are returned as relative paths under
//! the REP file DB root, otherwise only the base names are returned.
{
  return REP.Storage.pg_page_files (get_storage_path(), file_type, pathnames);
}

int|string|REP.Page get_page_file_subspec(string file_path)
{
  REP.Storage.FileType file_type = REP.Storage.type_from_filename(file_path);
  if (REP.Storage.is_page_specific_type(file_type)) {
    int page_id = REP.Storage.page_id_from_filename(file_path);
    return page_by_id(page_id, REP.LOG_ERROR);
  } else if (REP.Storage.is_layout_mime_specific_type(file_type)) {
    return low_mime_type (file_type);
  } else if (REP.Storage.is_text_chain_type(file_type)) {
    return REP.Storage.text_chain_uuid_from_filename(file_path);
  }

  //  Remaining types have no subspec
  return 0;
}

multiset(REP.Storage.FileType) list_file_types()
//! Returns the file types for all files that exists for this page
//! version.
{
  string dir = get_storage_path();
  array(string) list = get_db_dir (dir);
  if (!list) return (<>);

  multiset(FileType) res = (<>);
  foreach (list, string file)
    if (!has_suffix (file, "~")) {
      sscanf (file, "%[-a-z]", string file_prefix);
      if (FileType type = prefix_file_type[file_prefix])
	res[type] = 1;
    }

  return res;
}

//! Attempt to find the mime type of the layout file for this page
//! version. If no layout file exists, but an UNPROC_LAYOUT_FILE does
//! exist, the mime type will be extrapolated through
//! @[layout_filenames_by_mime]. If no layout file exists at all,
//! UNDEFINED is returned.
string layout_mime_type()
{
  if (string res = low_mime_type (LAYOUT_FILE))
    return res;

  if (mapping(FileType:string) filenames =
      layout_filenames_by_mime[unproc_mime_type()]) {
    string filename = filenames[LAYOUT_FILE];
    return mime_from_filename (filename);
  }

  return UNDEFINED;
}

//! Attempt to find the mime type of the UNPROC layout file for this
//! page version. If no UNPROC layout file exists, UNDEFINED is
//! returned.
string unproc_mime_type()
{
  return low_mime_type (UNPROC_LAYOUT_FILE);
}

protected mapping(FileType:string) _layout_mime_types = ([]);

protected string low_mime_type (FileType file_type)
{
  ASSERT_IF_DEBUG ((<UNPROC_LAYOUT_FILE, LAYOUT_FILE>)[file_type]);

  if (!_layout_mime_types[file_type]) {
    string dir = get_storage_path();

    foreach (get_db_dir (dir), string filename) {
      if (string mimetype = mime_from_filename(filename)) {
	array(string) parts = filename / ".";
	FileType _type = prefix_file_type[parts[0]];
	if ((<UNPROC_LAYOUT_FILE, LAYOUT_FILE>)[_type]) {
	  _layout_mime_types[_type] = mimetype;
	}
      }
    }
  }

  return _layout_mime_types[file_type];
}

//! Returns the filepath for the layout file, relative to the file db root.
//!
//! If no layout file exists, the error is signalled according to
//! @[file_missing].
//!
//! @note
//! This function doesn't return the path to an unprocessed layout
//! file. I.e. this function might fail even when @[layout_mime_type]
//! returns a mime type.
int(0..0)|string get_existing_layout_filepath (void|REP.OnError file_missing) {
  if (string mime_type = layout_mime_type()) {
    string path = get_layout_filepath (mime_type);
    if (REP.Storage.stat_db_file (path))
      return path;
  }
  return REP.raise_err (file_missing, "No layout file found for page group "
			"version %d in %O.\n", id(), get_storage_path());
}

string get_unproc_filepath(void|string layout_mime)
//! Returns the path under the REP file DB root for the unprocessed
//! layout file (@[REP.Storage.UNPROC_LAYOUT_FILE]).
//!
//! @param layout_mime
//! If given, returns the path for the specified MIME type, or zero if
//! the MIME type isn't known to be applicable for a layout file.
//!
//! If not given, the MIME type is taken from the existing layout file
//! (either unprocessed or processed). Zero is returned if none exists
//! for this page version.
{
  // It is meaningless to ask for the path for another mime type than
  // the one of the present file, since it can never be written to.
  ASSERT_IF_DEBUG (!layout_mime || !unproc_mime_type() ||
		   layout_mime /*%O*/ == unproc_mime_type() /*%O*/,
		   layout_mime, unproc_mime_type());

  mapping(FileType:string) filenames =
    layout_filenames_by_mime[layout_mime || unproc_mime_type()];
  if (!filenames) return 0;
  return combine_path(get_storage_path(), filenames[UNPROC_LAYOUT_FILE]);
}

string get_layout_filepath(void|string layout_mime)
//! Similar to @[get_unproc_filepath] but for
//! @[REP.Storage.LAYOUT_FILE].
{
  ASSERT_IF_DEBUG (!layout_mime || !layout_mime_type() ||
		   layout_mime /*%O*/ == layout_mime_type() /*%O*/,
		   layout_mime, layout_mime_type());
  mapping(FileType:string) filenames =
    layout_filenames_by_mime[layout_mime || layout_mime_type()];
  if (!filenames) return 0;
  return combine_path(get_storage_path(), filenames[LAYOUT_FILE]);
}

string get_split_layout_filepath (REP.Page page) {
  ASSERT_IF_DEBUG (layout_mime_type());
  ASSERT_IF_DEBUG (page->pgv() == this);
  return combine_path(get_storage_path(),
		      sprintf (filenames_subspec_by_type[SPLIT_LAYOUT_FILE],
			       page->get ("page_id"),
			       mime_to_extension[layout_mime_type()]));
}

string get_preview_filepath (REP.Page page) {
  ASSERT_IF_DEBUG (page->pgv() == this);
  return combine_path(get_storage_path(),
		      sprintf (filenames_subspec_by_type[PAGE_PREVIEW],
			       page->get ("page_id")));
}

// get_preview_big_filepath and get_preview_small_filepath are not yet
// needed.

string get_pdf_page_filepath (REP.Page page,
			      void|REP.Storage.FileType type)
{
  ASSERT_IF_DEBUG (page->pgv() == this);
  ASSERT_IF_DEBUG (!type ||
		   type == PDF_PAGE_PREVIEW ||
		   type == PDF_PAGE_PREVIEW_LR);
  if (!type) type = PDF_PAGE_PREVIEW;
  return combine_path(get_storage_path(),
		      sprintf (filenames_subspec_by_type[type],
			       page->get ("page_id")));
}

string get_pdf_group_filepath (void|REP.Storage.FileType type)
{
  ASSERT_IF_DEBUG (!type ||
		   type == PDF_GROUP_PREVIEW ||
		   type == PDF_GROUP_PREVIEW_LR);
  if (!type) type = PDF_GROUP_PREVIEW;
  return combine_path(get_storage_path(),
		      filenames_plain_by_type[type]);
}

//! Like @[get_page_filepath], but returns 0 if the file doesn't exist.
string get_existing_page_filepath (REP.Storage.FileType file_type,
				   void|REP.Page|int|string subspec)
{
  string path = get_page_filepath (file_type, subspec);

  if (!path) return 0;

  Stdio.Stat stat;
  if (!page_file_statcache)
    page_file_statcache = pg_volatile->page_file_statcache =
      (pg_volatile->page_file_statcache || ([]));

  if (zero_type (stat = page_file_statcache[path])) {
    stat = page_file_statcache[path] = REP.Storage.stat_db_file (path);
  }

  if ((< LAYOUT_FILE, PAGE_PREVIEW, PAGE_PREVIEW_BIG,
	 PAGE_PREVIEW_SMALL >)[file_type] &&
      !stat)
    process_ids (1);

  return stat && path;
}

string get_page_filepath (REP.Storage.FileType file_type,
			  void|REP.Page|int|string subspec,
			  void|int name_only)
//! Returns the path relative the REP file DB root for the file of the
//! given @[file_type]. If @[name_only] is set then only the file name
//! part is returned.
//!
//! @[subspec] depends on the @[file_type]:
//!
//! @ul
//! @item
//!   For files except SPLIT_LAYOUT_FILE that depend on the MIME type
//!   of the layout file, it can be a MIME type to use if there is no
//!   unprocessed layout file to tell it from. If there is no layout
//!   file and @[subspec] isn't given then zero is returned.
//! @item
//!   For page specific file types, including SPLIT_LAYOUT_FILE, it is
//!   the @[REP.Page] object.
//! @item
//!   For HEAD_SNIPPET and OVERFLOW_SNIPPET, it is the uuid of the text
//!   flow chain - see @[REP.TextFlowLink]. Note that the returned path
//!   may point to the storage dir of another version if the
//!   corresponding text flow link field already has a value.
//!   If @[name_only] is set then, it'll begin with "../".
//! @item
//!   For SPLIT_LAYOUT_FILE the path/name the file would have for the
//!   given subspec page is simply returned, without any intelligence
//!   for resolving the path for a spread etc. Use
//!   @[REP.Page.get_split_layout_filepath] as a higher level
//!   interface.
//! @item
//!   For the box preview file types, it is a document id integer.
//! @endul
{
  ASSERT_IF_DEBUG (is_valid_file_type (file_type));
  string path = name_only ? "" : get_storage_path();

  switch (file_type) {
    // File types that depend on the layout file mime type:
    case UNPROC_LAYOUT_FILE:
    case LAYOUT_FILE:
      string own_mimetype = (file_type == UNPROC_LAYOUT_FILE ?
			     unproc_mime_type() :
			     layout_mime_type());
      ASSERT_IF_DEBUG (!subspec || !own_mimetype ||
		       subspec /*%O*/ == own_mimetype /*%O*/,
		       subspec, own_mimetype);
      string mimetype = subspec || own_mimetype;
      if (!mimetype) return 0;

      ASSERT_IF_DEBUG (layout_filenames_by_mime[mimetype]);
      ASSERT_IF_DEBUG (layout_filenames_by_mime[mimetype][file_type]);
      return path + layout_filenames_by_mime[mimetype][file_type];

      // File types that take a page object:
    case PAGE_PREVIEW:
    case PAGE_PREVIEW_BIG:
    case PAGE_PREVIEW_SMALL:
    case PDF_PAGE_PREVIEW:
    case PDF_PAGE_PREVIEW_LR:
      ASSERT_IF_DEBUG (subspec->pgv() == this);
      return path + sprintf (filenames_subspec_by_type[file_type],
			     subspec->get ("page_id"));

    case HEAD_SNIPPET:
    case OVERFLOW_SNIPPET:
      ASSERT_IF_DEBUG (stringp (subspec));
      string filename = sprintf (filenames_subspec_by_type[file_type],
				 subspec);
      if (mapping(string:mixed) fields = versioned_tfl_by_uuid (subspec))
	if (int id = fields[file_type + "_ext"])
	  if (id != rec->storage_counter)
	    // Accessing an externally stored text flow link field which
	    // points to the storage dir of an older version.
	    return combine_path (path, "..", (string) id, filename);
      return path + filename;

    case SPLIT_LAYOUT_FILE:
      // This depends both on the layout file mime type and the page.
      ASSERT_IF_DEBUG (subspec->pgv() == this);
      mimetype = layout_mime_type();
      if (!mimetype) return 0;
      return path + sprintf (filenames_subspec_by_type[SPLIT_LAYOUT_FILE],
			     subspec->get ("page_id"),
			     mime_to_extension[mimetype]);

    default:
      return path + filenames_plain_by_type[file_type];
  }
}

class FileResult (REP.PGVersion pgv,
                  REP.Storage.FileType file_type,
                  void|REP.Page|int|string subspec)
{
  int(0..0)|Stdio.Stream get_stream()
  {
    return pgv->get_page_file_stream (file_type, subspec);
  }

  string get_filepath()
  {
    return pgv->get_page_filepath (file_type, subspec);
  }

  string mime_type()
  {
    return REP.Storage.mime_from_filename (get_filepath());
  }

  string ext_filename()
  {
    return REP.get_print_module()->get_pgv_ext_filename (pgv, subspec,
                                                         file_type);
  }

  string format_external_filename_wide (string format)
  {
    string extension = (get_filepath() / ".")[-1];
    REP.DBObject format_obj;
    if (subspec && subspec->is_rep_page)
      format_obj = subspec;
    else
      format_obj = pgv;

    return REP.Storage.format_external_filename_wide (format, format_obj) +
      "." + extension;
  }

  mapping(string:mixed) find_file_result(void|RequestID id)
  {
    string filepath = get_filepath();
    if (Stdio.File fd = get_stream()) {
      string mime_type = mime_from_filename (filepath);
      mapping ret = Roxen.http_file_answer (fd, mime_type);
      string dl_name = ext_filename();

      ret->extra_heads = ([
        "Content-Disposition": "attachment; filename=\"" +
        Protocols.HTTP.quoted_string_encode (string_to_utf8(dl_name)) + "\"",
      ]);

      if (id) {
        //  Don't use server-side cache for any resources since authentication
        //  requirements make them inaccessible anyway.
        id->misc->no_proto_cache = 1;

        //  Flag immutable preview images as fully cacheable client-side
        if (REP.Storage.is_jpeg_preview(file_type)) {
#ifdef CACHE_INDEF_REL_LIMIT
          id->raise_max_cache(CACHE_INDEF_REL_LIMIT);
#else
          id->raise_max_cache(86400 * 7);
#endif
        }
      }

      return ret;
    }

    return Roxen.http_status (Protocols.HTTP.HTTP_NOT_FOUND);
  }
}

//! Variant that can be used for non-persistent files.
class VolatileFileResult (REP.PGVersion pgv,
                          REP.Storage.FileType file_type,
                          void|REP.Page|int|string subspec,
                          string|Stdio.Stream data)
{
  inherit FileResult;

  Stdio.Stream get_stream()
  {
    return objectp(data) ? data : Stdio.FakeFile(data);
  }
}


protected mapping(string:array(Concurrent.Promise)) waiting_promises = ([]);

protected void add_waiting_promise (REP.Storage.FileType file_type,
                                    void|REP.Page|int|string subspec,
                                    Concurrent.Promise promise)
{
  string filename = get_page_filepath (file_type, subspec, 1);
  waiting_promises[filename] = waiting_promises[filename] || ({});
  waiting_promises[filename] += ({ promise });

  // Prevent races.
  if (is_valid_file (file_type, subspec))
    notify_waiting_promises (file_type, subspec);
}

protected void cancel_all_waiting_promises()
{
  if (!sizeof(waiting_promises))
    return;

  mixed cancel_err =
    Error.mkerror(sprintf("File generation failed for %O -- "
                          "PGVersion no longer valid.\n", this));
  foreach (waiting_promises; string filename; ) {
    if (array(Concurrent.Promise) ps = m_delete(waiting_promises, filename)) {
      foreach (ps, Concurrent.Promise p) {
#ifdef DEBUG
        werror("Cancelling PGV promise: %O, %O -> %O\n", this, filename, p);
#endif
        catch { p->failure(cancel_err); };
      }
    }
  }
}

void notify_waiting_promises (REP.Storage.FileType file_type,
                              void|REP.Page|int|string subspec)
{
  string filename = get_page_filepath (file_type, subspec, 1);
  array(Concurrent.Promise) promises = m_delete (waiting_promises, filename);

  if (!promises || !sizeof (promises)) return;

  int success;
  if (is_valid_file (file_type, subspec)) {
    success = 1;
  }

  FileResult res = FileResult (this, file_type, subspec);

  foreach (promises, Concurrent.Promise promise) {
    if (success)
      promise->success (res);
    else
      promise->failure (
        Error.mkerror(sprintf("File generation failed for %O:%O:%O.\n",
                              this, file_type, subspec)));
  }
}

void send_waiting_download (FileResult file_result, RequestID id)
{
  // Reset handle_time and handle_vtime since we're sending the result
  // of an asynchronous operation.
  id->handle_time = gethrtime();
#if constant(System.CPU_TIME_IS_THREAD_LOCAL)
  id->handle_vtime = gethrvtime();
#endif

  if (!file_result) {
    id->send_result (Roxen.http_status (Protocols.HTTP.HTTP_NOT_FOUND));
    return;
  }

  REP.Storage.FileType file_type = file_result->file_type;
  mixed/*void|REP.Page|int|string*/ subspec = file_result->subspec;

  int success;
  if (is_valid_file (file_type, subspec)) {
    success = 1;
  }

  mapping res = file_result->find_file_result(id);

  id->send_result (res);

  if (!success && is_pdf_preview (file_type)) {
    if (int user_id = REP.get_session()->mac->id_get_id (id)) {
      notify_user_pdf_failed ((< user_id >), file_type, subspec);
    }
  }
}

void notify_user_pdf_failed (multiset(int) notify_user_ids,
			     REP.Storage.FileType file_type,
			     void|REP.Page|int|string subspec,
			     void|string error_desc)
{
  REP.Edition edition = edition();
  int edition_id = edition->id();
  REP.PageSlot ps = subspec ? subspec->page_slot() : first_page_slot();
  string ps_name = (subspec && ps) ? ps->page_name() : page_names(1);
  string pg_link = sprintf("<a href='go:%d,%d,%d'><u>%s, page %s</u></a>",
			   edition->publication()->id(),
			   edition_id,
			   ps && ps->id(),
			   Roxen.html_encode_string(edition->get("title")),
			   Roxen.html_encode_string(ps_name));
  string msg_body = "PDF failed for " + pg_link + ".";

  if (error_desc)
    msg_body += " " + Roxen.html_encode_string (error_desc);

  //  Always include admin users in the notification in case there is a
  //  configuration error to resolve.
  multiset(int) admin_users = (multiset) REP.AC.get_rep_admin_users()->id();
  notify_user_ids = (notify_user_ids || (< >) ) | admin_users;

  string msg_hash =
    sprintf("page-pdf|%d|%d", edition_id, get_unver("page_group_id"));
  mapping info =
    ([ "msg": ([ "group":     "PDF Generation",
		 "title":     "PDF failed.",
		 "class":     "warn pdf",
		 "body_html": msg_body,
		 "hash":      msg_hash ]) ]);
  REP.Notification.Client.send_user_message(info, notify_user_ids);
}

mapping find_file (REP.Storage.FileType file_type,
		   void|REP.Page|int|string subspec,
		   RequestID id,
                   void|int(0..1) upconvert)
{
  if (mapping res = REP.AC.verify_dbobj_request_perm (this, id))
    return res;

  Concurrent.Future future;

  if (file_type == REP.Storage.LAYOUT_FILE) {
    if (upconvert && get_existing_page_filepath (file_type, subspec) &&
        !REP.IDS.is_compatible_version (get ("indesign_version"))) {
      // Perform on-demand upconversion. The result will only be sent
      // in the request, not saved locally. Once the user saves the
      // document back to the server, it will have the correct
      // version. This saves us some headaches regarding versioning,
      // etc.
      Concurrent.Promise promise = Concurrent.Promise();
      future = promise->future();
      void resaved_callback (string|Stdio.Stream data)
      {
        if (!stringp (data)) data = data->read(0x7fffffff);
        mapping(string:string) upconvert_checksums =
          low_get_file_checksums (data);
        foreach (low_get_page_file_checksums(file_type);
                 string idx; string val) {
          add_checksum_remap(val, upconvert_checksums[idx]);
        }
        promise->success (VolatileFileResult (this, file_type, subspec, data));
      };

      REP.IDS.ids_module()->upgrade_document (this, resaved_callback);
    }
  }

  if (!future)
    future = get_existing_page_file (file_type, subspec);

  //  If we already have a successful result ready, return it directly.
  //  Note that try_get() will throw an exception if the future has been
  //  rejected, so in that case we simply skip this optimization.
  catch {
    if (FileResult res = future->try_get()) {
      future = 0;
      return res->find_file_result(id);
    }
  };

  mixed co_handle;

  function(FileResult:void) success_callback =
    lambda (FileResult res)
    {
      remove_call_out (co_handle);
      if (!res) {
        werror ("%O: Didn't get result for %O (%O) upconvert:%d\n",
                this, file_type, subspec, upconvert);
      }
      if (id) {
        if (res) {
          res->pgv->send_waiting_download (res, id);
        } else {
          send_waiting_download (0, id);
        }
        id = 0;
      }
    };

  function(mixed:void) failure_callback =
    lambda (mixed err)
    {
      werror ("%O->find_file(%O, %O, %O, %O) error:\n%s\n",
              this, file_type, subspec, id, upconvert, describe_error (err));
      if (id) {
        send_waiting_download (0, id);
        id = 0;
      }
    };

  // Set timeout.
  co_handle = roxen.background_run (60,
                                    failure_callback,
                                    Error.mkerror("Promise timeout"));

  future->on_success (REP.cb_wrapper (success_callback));
  future->on_failure (REP.cb_wrapper (failure_callback));

  // Avoid cyclic trampoline garbage.
  success_callback = 0;
  failure_callback = 0;
  future = 0;

  return Roxen.http_pipe_in_progress();
}

//! Returns a @[FileResult] it the requested file exist, else UNDEFINED is
//! returned.
FileResult get_page_file(REP.Storage.FileType file_type,
                         void|REP.Page|int|string subspec)
{
  if (is_valid_file(file_type, subspec)) {
    return FileResult(this, file_type, subspec);
  }
  return UNDEFINED;
}

//! Returns a Concurrent.Future that resolves with this @[REP.PGVersion] when
//! it is fully processed by the InDesign server, i.e. a
//! @[REP.Storage.LAYOUT_FILE] exists, preflight info is collected,
//! @[REP.AdPlacement] objects have been extracted, etc.
Concurrent.Future/*REP.PGVersion*/ await_processing()
{
  // FIXME: Verify that everything else really is processed when the LAYOUT_FILE
  // appears.
  return get_existing_page_file (REP.Storage.LAYOUT_FILE)
    ->map(lambda() { return this; });
}


//!  Only to be called internally. We cannot declare it protected since we
//!  might call into other PGV objects below.
/*protected*/ Concurrent.Future/*REP.PGVersion.FileResult*/
get_existing_page_file_low (REP.Storage.FileType file_type,
                            void|REP.Page|int|string subspec,
                            Concurrent.Promise|void promise)
{
  promise = promise || Concurrent.Promise();

  if (is_valid_file (file_type, subspec)) {
    promise->success (FileResult (this, file_type, subspec));
  } else if (get_existing_page_filepath (REP.Storage.LAYOUT_FILE) ||
             get_existing_page_filepath (REP.Storage.UNPROC_LAYOUT_FILE)) {
    REP.PGVersion gen_pgv = generate_file (file_type);
    if (gen_pgv != this) {
      if (is_page_specific_type (file_type))
	subspec = gen_pgv->page_by_id (subspec->get ("page_id"));
      return gen_pgv->get_existing_page_file_low (file_type, subspec, promise);
    }
    gen_pgv = 0;

    add_waiting_promise (file_type, subspec, promise);
  } else {
    promise->failure (Error.mkerror(sprintf("No layout file available for %O\n",
                                            this)));
  }

  return promise->future();
}

// FIXME: Calling get() on the returned future may hang forever if the file
//        never gets generated, e.g. if the InDesign server is not available.
//        Fix this!
//
//! Returns a @[Concurrent.Future] that will be fulfilled with a
//! @[FileResult]. Note that the @[FileResult] may reference a
//! different @[PGVersion] than the one this method was called in,
//! e.g. if a new version was required to regenerate a stale file.
Concurrent.Future/*REP.PGVersion.FileResult*/
get_existing_page_file (REP.Storage.FileType file_type,
                        void|REP.Page|int|string subspec)
{
  return get_existing_page_file_low(file_type, subspec);
}


int|Stdio.File get_page_file_stream (REP.Storage.FileType file_type,
				     void|REP.Page|int|string subspec,
				     void|REP.OnError on_io_err)
//! Returns the page file specified by @[file_type] and @[subspec]
//! opened for reading. See @[get_page_filepath] for more info on
//! those arguments.
//!
//! @param on_io_err
//!   How to react if there is an I/O error opening the file. (However,
//!   if the file doesn't exist, zero will be returned.)
{
  string path = get_page_filepath (file_type, subspec);
  return path && REP.Storage.read_open_db_file (path, 1, on_io_err);
}

int|string get_page_file_data (REP.Storage.FileType file_type,
			       void|REP.Page|int|string subspec,
			       void|REP.OnError on_io_err)
//! Returns the contents of the page file specified by @[file_type]
//! and @[subspec] in string form. See @[get_page_filepath] for more
//! info on those arguments.
//!
//! @param on_io_err
//!   How to react if there is an I/O error opening the file. (However,
//!   if the file doesn't exist, zero will be returned.)
{
  string path = get_page_filepath (file_type, subspec);
  return path && REP.Storage.read_db_file (path, 1, on_io_err);
}

int|mapping(string:string)
get_page_file_checksums(REP.Storage.FileType file_type,
			void|REP.Page|int|string subspec,
			void|REP.OnError on_io_err)
//! Returns checksums of the contents of the page file specified by
//! @[file_type] and @[subspec] in string form. See @[get_page_filepath]
//! for more info on those arguments. If checksums have been computed
//! earlier they are retrieved from properties and the file is not opened.
//! The function currently only supports @[REP.Storage.UNPROC_LAYOUT_FILE]
//! and @[REP.Storage.LAYOUT_FILE] and a ©[subspec] of zero.
//!
//! @param on_io_err
//!   How to react if there is an I/O error opening the file.
//!
//! @returns
//! Returns a mapping with two or three items:
//! @ul
//! @item "md5"
//!   The file's MD5 checksum as hex string in lowercase.
//!
//! @item "crc32"
//!   The file's CRC32 checksum as hex string in lowercase zero-padded
//!   to 8 characters.
//!
//! @item "sha256"
//!   The file's SHA256 checksum as hex string in lowercase.
//!
//! @item "data"
//!   The file data. Will be zero if cached checksums are used. The data
//!   is included only as an optimization.
//! @endul
{
  ASSERT_IF_DEBUG(file_type == REP.Storage.UNPROC_LAYOUT_FILE ||
		  file_type == REP.Storage.LAYOUT_FILE);
  ASSERT_IF_DEBUG(!subspec);
  return low_get_page_file_checksums(file_type, subspec, on_io_err, 0);
}

protected int|mapping(string:string)
low_get_page_file_checksums(REP.Storage.FileType file_type,
			    void|REP.Page|int|string subspec,
			    void|REP.OnError on_io_err,
			    void|string|Stdio.Stream upload_data)
{
  if (file_type != REP.Storage.UNPROC_LAYOUT_FILE &&
      file_type != REP.Storage.LAYOUT_FILE)
    return 0;

  //  Check for cached value in property

  int|string file_data;
  mapping(string:string) info =
    ([ "md5"   : get_file_metadata (file_type, 0, "md5"),
       "crc32" : get_file_metadata (file_type, 0, "crc32"),
       "sha256": get_file_metadata (file_type, 0, "sha256") ]);

  if (!info->md5 || !info->crc32 || !info->sha256) {
    //  Try to get data from caller
    file_data =
      (stringp(upload_data)) ?
      upload_data :
      get_page_file_data(file_type, subspec, on_io_err);

    //  Update checksums
    if (intp(file_data))
      return file_data;

    info += low_get_file_checksums(file_data);

    DisableAutoCommit dac = DisableAutoCommit();
    set_file_metadata (file_type, 0, "md5", info->md5);
    set_file_metadata (file_type, 0, "crc32", info->crc32);
    set_file_metadata (file_type, 0, "sha256", info->sha256);
    destruct(dac);
  }
  return info;
}

protected mapping(string:string)
low_get_file_checksums(string file_data)
{
  mapping(string:string) info = ([]);
  info->data = file_data;
  info->md5 = String.string2hex(Crypto.MD5.hash(file_data));
  int crc32 = Gz.crc32(file_data);
  if (crc32 < 0)
    crc32 += 0x100000000;
  info->crc32 = sprintf("%08x", crc32);
  info->sha256 = String.string2hex(Crypto.SHA256.hash(file_data));

  return info;
}

protected void clear_page_file_checksums(REP.Storage.FileType file_type,
					 void|REP.Page|int|string subspec)
{
  if ((< UNPROC_LAYOUT_FILE, LAYOUT_FILE >)[file_type]) {
    DisableAutoCommit dac = DisableAutoCommit();
    set_file_metadata (file_type, 0, "md5", UNDEFINED);
    set_file_metadata (file_type, 0, "crc32", UNDEFINED);
    set_file_metadata (file_type, 0, "sha256", UNDEFINED);
    destruct(dac);
  }
}

int(0..1) store_file (string|Stdio.Stream data,
		      REP.Storage.FileType file_type,
		      void|string|REP.Page|int subspec,
		      void|REP.OnError on_io_err,
		      void|string|REP.PGVersion source,
		      void|int allow_replace,
                      void|bool skip_shadow_copy)
//! Writes the given data to the file specified by @[file_type] and
//! @[subspec], and trigs notifications about it.
//!
//! @[file_type] and @[subspec] specifies the file in the same way as
//! @[get_page_filepath].
//!
//! @[source] is an optional specifier to identify the source or cause
//! of the file store. Currently required iff @[file_type] ==
//! @[REP.Storage.UNPROC_LAYOUT_FILE]; see
//! @[PrintDBModule.notify_page_file_added] for details.
//!
//! If the file exists already then it is assumed to have been created
//! due to a race and hence be identical to @[data]. In that case
//! nothing is done, and the function returns zero (regardless of
//! @[on_io_err]). This behaviour can be overridden by specifying a
//! nonzero @[allow_replace] argument.
//!
//! @[on_io_err] specifies how to handle I/O errors - see
//! @[REP.Storage.save_db_file].
//!
//! @note
//!   Unprocessed layout files (i.e. @[file_type] is
//!   @[REP.Storage.UNPROC_LAYOUT_FILE]) cannot be stored in
//!   @[REP.PGVersion] objects that have been made current.
{
  ASSERT_DBOBJECT_WRITE_PERM;
  // Cannot store an UNPROC_LAYOUT_FILE in a live PGVersion.
  ASSERT_IF_DEBUG (file_type != REP.Storage.UNPROC_LAYOUT_FILE || !is_live);

  ASSERT_IF_DEBUG (file_type == REP.Storage.UNPROC_LAYOUT_FILE ?
		   source : !source);

  string filepath = get_page_filepath (file_type, subspec);
  ASSERT_IF_DEBUG (filepath);
  if (!tbl->print_db->save_db_file (filepath, data, allow_replace ? 1 : -1,
				    on_io_err))
    return 0;

  if (allow_replace)
    clear_page_file_checksums(file_type, subspec);
  low_get_page_file_checksums(file_type, subspec, REP.RETURN_ZERO, data);
  notify_page_file_added (file_type, subspec, source, skip_shadow_copy);
  return 1;
}

int(0..1) store_file_by_move (string src_filepath,
			      REP.Storage.FileType file_type,
			      void|string|REP.Page|int subspec,
			      void|int allow_nonexisting,
			      void|REP.OnError on_io_err,
			      void|string|REP.PGVersion source,
			      void|int allow_replace,
                              void|bool skip_shadow_copy)
//! Similar to @[store_file], but gets the source by moving in the
//! file at @[src_filepath], which is not relative to the REP file DB
//! root.
//!
//! If the destination file exists already then it is assumed to have
//! been created due to a race and hence be identical to
//! @[src_filepath]. In that case @[src_filepath] is simply removed
//! without trigging notifications, and the function returns zero
//! (regardless of @[on_io_err]). This behaviour can be overridden by
//! specifying a nonzero @[allow_replace] argument.
//!
//! @[allow_nonexisting] and @[on_io_err] are passed on to
//! @[REP.Storage.move_db_file] - see it for details.
//!
//! For the other arguments, see @[store_file] for details.
{
  ASSERT_DBOBJECT_WRITE_PERM;
  // Cannot store an UNPROC_LAYOUT_FILE in a live PGVersion.
  ASSERT_IF_DEBUG (file_type != REP.Storage.UNPROC_LAYOUT_FILE || !is_live);

  ASSERT_IF_DEBUG (file_type == REP.Storage.UNPROC_LAYOUT_FILE ?
		   source : !source);

  string dst_filepath = get_page_filepath (file_type, subspec);
  ASSERT_IF_DEBUG (dst_filepath);
  if (!tbl->print_db->move_db_file (dst_filepath, src_filepath,
				    allow_nonexisting, allow_replace ? 1 : -1,
				    on_io_err))
    return 0;

  clear_page_file_checksums(file_type, subspec);
  notify_page_file_added (file_type, subspec, source, skip_shadow_copy);
  return 1;
}

int(0..1) store_file_by_copy (string src_filepath,
			      REP.Storage.FileType file_type,
			      void|string|REP.Page|int subspec,
			      void|int allow_nonexisting,
			      void|REP.OnError on_io_err,
			      void|string|REP.PGVersion source,
			      void|int allow_replace,
                              void|bool skip_shadow_copy)
//! Similar to @[store_file], but gets the source by copying, or if
//! possible hardlinking, the file at @[src_filepath], which is not
//! relative to the REP file DB root.
//!
//! If the destination file exists already then it is assumed to have
//! been created due to a race and hence be identical to
//! @[src_filepath]. In that case the function just returns zero
//! (regardless of @[on_io_err]) without trigging notifications.
//! This behaviour can be overridden by specifying a nonzero
//! @[allow_replace] argument.
//!
//! @[allow_nonexisting] and @[on_io_err] are passed on to
//! @[REP.Storage.copy_db_file] - see it for details.
//!
//! For the other arguments, see @[store_file] for details.
{
  ASSERT_DBOBJECT_WRITE_PERM;
  // Cannot store an UNPROC_LAYOUT_FILE in a live PGVersion.
  ASSERT_IF_DEBUG (file_type != REP.Storage.UNPROC_LAYOUT_FILE || !is_live);

  ASSERT_IF_DEBUG (file_type == REP.Storage.UNPROC_LAYOUT_FILE ?
		   source : !source);

  string dst_filepath = get_page_filepath (file_type, subspec);
  ASSERT_IF_DEBUG (dst_filepath);
  if (!tbl->print_db->copy_db_file (dst_filepath, src_filepath,
				    allow_nonexisting, allow_replace ? 1 : -1,
				    on_io_err))
    return 0;

  clear_page_file_checksums(file_type, subspec);
  notify_page_file_added (file_type, subspec, source, skip_shadow_copy);
  return 1;
}

void set_file_metadata (REP.Storage.FileType file_type,
			REP.Page|string|int|void subspec,
			string index,
			mixed value)
{
  ASSERT_DBOBJECT_WRITE_PERM;
  ASSERT_IF_DEBUG (avail_file_md_fields[file_type] &&
		   avail_file_md_fields[file_type][index]);
  string filename = get_page_filepath (file_type, subspec, 1);
  ASSERT_IF_DEBUG (filename);
  int changed;

  // This part is supposed to be race-free since we're referencing the
  // rec->file_md mapping directly (but we'll set it through
  // set_fields in the end to get the DB commit right).
  if (!rec->file_md) rec->file_md = ([]);
  mapping(string:mapping) file_md = rec->file_md;
  if (!file_md[filename]) file_md[filename] = ([]);
  if (!equal (file_md[filename][index], value)) {
    file_md[filename][index] = value;
    changed = 1;
  }

  if (changed) {
    dirty_fields->file_md = 1;
    if (!auto_commit_disabled_count)
      commit();
  }
}

mixed get_file_metadata (REP.Storage.FileType file_type,
			 REP.Page|string|int|void subspec,
			 string index)
{
  ASSERT_IF_DEBUG (avail_file_md_fields[file_type] &&
		   avail_file_md_fields[file_type][index]);
  // Note: we may not get a filename, if file_type is *LAYOUT_FILE and
  // a content type isn't specified in subspec.
  string filename = get_page_filepath (file_type, subspec, 1);
  return rec->file_md && rec->file_md[filename] &&
    rec->file_md[filename][index];
}

int(0..1) is_stale_file (REP.Storage.FileType file_type,
			 REP.Page|string|int|void subspec)
//! Returns 1 iff the specified file _exists_ but is stale.
{
  //  Both PDF and package files depend on the publication's PDF generation
  if (is_pdf_preview (file_type) ||
      file_type == REP.Storage.PACKAGE_FILE) {
    return get_existing_page_filepath (file_type, subspec) &&
      get_file_metadata (file_type, subspec, "pdf_settings_gen") !=
      REP.DB.pdf_settings_generation (edition()->publication());
  }

  return 0;
}

int(0..1) is_valid_file (REP.Storage.FileType file_type,
			 REP.Page|string|int|void subspec)
//! Returns 1 iff the specified file exists and isn't stale.
{
  return get_existing_page_filepath (file_type, subspec) &&
    !is_stale_file (file_type, subspec);
}

protected multiset(REP.Storage.FileType) file_types_in_progress = (<>);

REP.PGVersion generate_file (REP.Storage.FileType file_type)
{
  if (file_types_in_progress[file_type]) return this;

  int is_valid = 1;
  int is_stale;
  if (is_page_specific_type (file_type)) {
    foreach (pages(), REP.Page page) {
      is_stale |= is_stale_file (file_type, page);
      is_valid &= is_valid_file (file_type, page);
    }
  } else {
    is_stale = is_stale_file (file_type);
    is_valid &= is_valid_file (file_type);
  }

  if (is_valid)
    return this;

  file_types_in_progress[file_type] = 1;

  if (is_stale) {
    // The file is stale - need to create a new version.
    REP.PGVersion gen_pgv;
    REP.PGVersion cur_pgv;
    do {
      cur_pgv = current_pgv();

      multiset(REP.Storage.FileType) exclude_types = (<>);
      foreach (cur_pgv->list_file_types(); REP.Storage.FileType t;) {
	int t_stale;
	if (is_page_specific_type (t)) {
	  foreach (pages(), REP.Page page) {
	    t_stale |= cur_pgv->is_stale_file (t, page);
	  }
	} else {
	  t_stale = cur_pgv->is_stale_file (t);
	}
	if (t_stale)
	  exclude_types[t] = 1;
      }
      gen_pgv = REP.DB.add_pgv_except_files (cur_pgv, exclude_types);
    } while (!REP.DB.make_pgv_current (gen_pgv, cur_pgv, REP.RETURN_ZERO));
    file_types_in_progress[file_type] = 0;
    return gen_pgv->generate_file (file_type);
  }

  if (is_pdf_preview (file_type) &&
      file_type != REP.Storage.PDF_ISSUE_PREVIEW) {
    REP.IDS.generate_pdfs (this, ({ file_type }));
  } else if (file_type == REP.Storage.PACKAGE_FILE) {
    get_existing_page_file (REP.Storage.LAYOUT_FILE)
      ->on_success (REP.cb_wrapper (lambda (FileResult res)
                    {
                      REP.IDS.generate_layout_file_package (this);
                    }))
      ->on_failure(REP.cb_wrapper(lambda(mixed ignored) {
				    notify_waiting_promises(file_type);
				  }));
  } else if (file_type == REP.Storage.IDML_EXPORT_FILE) {
    get_existing_page_file (REP.Storage.LAYOUT_FILE)
      ->on_success (REP.cb_wrapper (lambda (FileResult res)
                    {
                      REP.IDS.generate_idml_export (this);
                    }))
      ->on_failure(REP.cb_wrapper(lambda(mixed ignored) {
				    notify_waiting_promises(file_type);
				  }));
  } else if ((is_jpeg_preview (file_type) ||
              file_type == REP.Storage.LAYOUT_FILE) &&
             get_existing_page_filepath (REP.Storage.UNPROC_LAYOUT_FILE)) {
    process_ids();
  }

  return this;
}

// Calls @[func] for each PGVersion having the same storage counter as this
// object, including this object itself.
void process_shared_storage_pgvs(function(REP.PGVersion:void) func)
{
  array(REP.PGVersion) processed = ({});
  array(REP.PGVersion) current_set =
    REP.DB.pgvs_by_storage_counter (rec->storage_counter);
  array(REP.PGVersion) batch = current_set - processed;

  while (sizeof(batch)) {
    foreach(batch, REP.PGVersion pgv) {
      func(pgv);
    }
    processed += batch;
    // Make sure to process any version that was added while processing the
    // others.
    current_set = REP.DB.pgvs_by_storage_counter (rec->storage_counter);
    batch = current_set - processed;
  }
}

protected void
notify_storage_counter_promises (REP.Storage.FileType file_type,
                                 REP.Page|string|int|void subspec)
{
  // Notify all pgvs with the same storage counter.
  process_shared_storage_pgvs(lambda(REP.PGVersion pgv)
  {
    REP.Page loop_subspec = subspec;

    if (REP.Storage.is_page_specific_type (file_type) && objectp (subspec)) {
      int page_id = subspec->id();

      // Lookup page object in the correct pgv.
      if (REP.Page p = pgv->page_by_id (page_id, REP.RETURN_ZERO)) {
	p->clear_client_rec_cache();
        loop_subspec = p;
      }
    }

    REP.get_session()->register_session_done_callback (
      lambda(REP.REPSession ses,
             REP.Storage.FileType file_type,
             REP.Page|string|int|void subspec,
             REP.PGVersion pgv)
      {
        pgv->notify_waiting_promises (file_type, subspec);
      }, file_type, loop_subspec, pgv);
  });
}

protected void notify_page_file_added (REP.Storage.FileType file_type,
				       REP.Page|string|int|void subspec,
				       REP.PGVersion source,
                                       bool skip_shadow_copy)
{
  if (page_file_statcache = pg_volatile->page_file_statcache)
    m_delete (page_file_statcache, get_page_filepath (file_type, subspec));

  file_types_in_progress[file_type] = 0;

  switch (file_type) {
  case UNPROC_LAYOUT_FILE:
    process_ids();
    perform_pending_operations (this, ({ file_type }));
    m_delete (cached_rec_from_get_ext, "preflight_results_txt");
    break;

  case LAYOUT_FILE:
    perform_pending_operations (this, ({ file_type }));
    check_current_layout_invalidation();
    m_delete (cached_rec_from_get_ext, "preflight_results_txt");
    break;

  default:
    if (is_jpeg_preview(file_type)) {
      //  Get image dimensions and store as metadata
      if (int|Stdio.File jpeg_fd =
	  get_page_file_stream(file_type, subspec, REP.RETURN_ZERO)) {
	array(string|int) dims = REP.image_dims(jpeg_fd);
	jpeg_fd->close();
	if (arrayp(dims))
	  set_file_metadata(file_type, subspec, "dims", dims[0..1]);
      }
    }

    //  We let both PDF and package file depend on the publication's PDF
    //  settings generation so they can be invalidated together.
    if (is_pdf_preview (file_type) ||
	file_type == REP.Storage.PACKAGE_FILE) {
      REP.Publication publ = edition()->publication();
      set_file_metadata (file_type, subspec, "pdf_settings_gen",
			 REP.DB.pdf_settings_generation (publ));
    }
  }

  m_delete (cached_rec_from_get_ext, "file_urls");

  notify_storage_counter_promises (file_type, subspec);

  tbl->print_db->notify_page_file_added (this, file_type, subspec, source);

  if (!skip_shadow_copy) {
    if ((file_type == UNPROC_LAYOUT_FILE) ||
        ((file_type == LAYOUT_FILE) && !unproc_mime_type()))  {
      REP.DB.queue_update_shadow_pgvs( ({ this }) );
    }
  }
}

//! Convenience function to notify about page file creation failures.
void notify_page_file_failed(REP.Storage.FileType file_type,
			     REP.Page|string|int|void subspec,
			     void|string error_desc)
{
  file_types_in_progress[file_type] = 0;

  notify_storage_counter_promises (file_type, subspec);

  if (is_pdf_preview (file_type))
    notify_user_pdf_failed ((< get ("user_id") >), file_type, subspec,
			    error_desc);

  tbl->print_db->notify_page_file_failed(this, file_type, subspec);
}


Concurrent.Future/*array(REP.PGVersion)*/
update_shadowed_slots(void|mapping(string|REP.Edition:int/*REP.DB.ShadowCopyPolicy*/)
                      dst_editions)
{
  //  *** FIXME [ZE]: Acquire locks to all shadow PGVs before starting?
  //  Otherwise we're race-prone with regards to concurrent unlinking/
  //  relinking, and potentially ad updates as well (though the latter
  //  will(?)/could be queued like for regularly locked page groups.
  //
  //  Alternatively we might keep a list of syncs in flight and check
  //  against that in unlink/relink methods in REP.PageSlot. But what
  //  about ad updates then?
  //
  //  Yet another method would be to keep shadow locks in sync with the
  //  top-most ancestor at all times, but that would prevent unlinking
  //  from a page that's locked for a long duration.
  //
  //
  //  Change to unlocked PGV
  //   - proceed immediately to shadow sync step
  //
  //  Change to locked PGV
  //   - add unlock callback which checks for PGV change; exit early
  //     if nothing happened (RAL upload that happens after lock release
  //     will fall in unlock category above).
  //
  //  Shadow sync
  //   - find closure of Z/E children (or just E children if self is
  //     shadow to allow ad propagation), and exclude locked editionsn
  //   - lock all of them to prevent races
  //   - run shadow copy routine that creates new PGVs
  //   - unlock all WITHOUT triggering change callback recursively (hmm)
  //
  //
  //  *** ZONE-SPECIFIC AD BOOKINGS ***
  //
  //   - When ENABLED, we could optimize away CROSS-ZONE copies if the PGV
  //     change is initiated by the ad manager. NOTE: requires a different
  //     traversal strategy to better identify intra-zone copies unlike now
  //     when everything is copied from top-level master, but locked editions
  //     will complicate the optimization since we must always copy from a
  //     source in an unlocked edition.
  //
  //   - When DISABLED, we must skip the IDS ad layer preservation that
  //     currently happens. This means the same page ads are reused
  //     everywhere and thus can be copied from top-level master if easier.

  //  Default shadow copy policy should ideally depend on whether each zone
  //  has its own ad plan or if it's shared with master. This is currently
  //  not configurable, but we'll pretend it is. This default may be
  //  overridden per edition by the caller, e.g. when branching a new zone
  //  or edition.
  REP.DB.ShadowCopyPolicy default_shadow_copy_policy =
    REP.DB.DEFAULT_SHADOW_POLICY;

  //  Prefer UNPROC_LAYOUT_FILE with fallback to LAYOUT_FILE
  REP.Storage.FileType file_type =
    (low_mime_type(UNPROC_LAYOUT_FILE) && UNPROC_LAYOUT_FILE) ||
    (low_mime_type(LAYOUT_FILE) && LAYOUT_FILE);
  if (!file_type)
    return Concurrent.resolve( ({ }) );

  //  Loop over each page slot and aggregate all shadowed PGVs that need to
  //  be updated.
  mapping(REP.PGVersion:REP.DB.ShadowCopyPolicy) shadow_pgvs = ([ ]);
  mapping(REP.Edition:array(REP.PageSlot)) shadow_pss_per_edition = ([ ]);
  array(REP.PageSlot) pss = pages()->page_slot() - ({ 0 });
  foreach (pss; int doc_idx; REP.PageSlot ps) {
    array(REP.PageSlot) shadow_pss = ps->shadowed_slots(true);
    foreach (shadow_pss, REP.PageSlot shadow_ps) {
      //  If a targeted edition is given we ignore all other shadow slots.
      //  This may also override the default copy policy.
      REP.Edition shadow_ed = shadow_ps->edition();
      REP.DB.ShadowCopyPolicy override_policy;
      if (dst_editions) {
        override_policy =
          dst_editions[shadow_ed] ||
          dst_editions["*"];
        if (!override_policy)
          continue;
      }

      //  Check for write permssion on this destination slot. If the Z/E has
      //  been locked the copy would fail anyway.
      if (shadow_ps->access_perm() < AC.PERM_WRITE)
        continue;

      //  Candidate for update. Collect all slots per edition. Since we
      //  loop over the master in page order the shadow slots will be
      //  assigned in correponding order per edition.
      if (!shadow_pss_per_edition[shadow_ed]) {
        //  First occurrence of a relationship for this edition so find
        //  which PGV we should update.
        if (REP.Page shadow_page = shadow_ps->page()) {
          if (REP.PGVersion shadow_pgv = shadow_page->pgv()) {
            shadow_pgvs[shadow_pgv] =
              override_policy ||
              default_shadow_copy_policy;
            shadow_pss_per_edition[shadow_ed] = ({ });
          }
        }
      }

      //  Map the slots. If the source doc index isn't in sync we add
      //  placeholders so we can handle missing page shadows.
      if (array(REP.PageSlot) pss = shadow_pss_per_edition[shadow_ed]) {
        while (sizeof(pss) < (doc_idx - 1))
          pss += ({ 0 });
        pss += ({ shadow_ps });
        shadow_pss_per_edition[shadow_ed] = pss;
      }
    }
  }

  //  Queue the copy for each target. This will attempt to lock the target
  //  first and if not possible defer the operation until the page is
  //  unlocked again.
  array(Concurrent.Future/*REP.PGVersion*/) futures = ({ });
  foreach (shadow_pgvs; REP.PGVersion shadow_pgv;
           REP.DB.ShadowCopyPolicy policy) {
    //  Confirm that the shadow is visible in a page plan
    if (shadow_pgv->is_placed()) {
      //  Perform the shadow copy which preserves any Ads layer currently
      //  found in the shadow PGV.
      //
      //  NOTE: This depends on the shadow being the main source of ad
      //  placements since we don't generally have any other backing store
      //  to redraw the ads from if this data is lost. It's therefore
      //  crucial that this operation succeeds or fails as a whole (e.g. if
      //  IDS processing is interrupted) so the previous shadow PGV can
      //  remain available for a new attempt later.

      //  Resolve any default policy to a reasonable behavior based on how
      //  cross-zone ads are managed.
      //
      //  *** FIXME [ZE]: Should this be controlled in defvar?

      REP.PGVersion ad_init_pgv;
    decide_default_policy:
      if (policy == REP.DB.DEFAULT_SHADOW_POLICY) {
        //  Special case for zoned pages that share ad external source ID
        //  which means they should be copied from the master edition.
        if (REP.PGVersion master_pgv = shadow_pgv->get_synced_ads_master()) {
          policy = REP.DB.OVERWRITE_ALL;
          break decide_default_policy;
        }

        policy = REP.DB.PRESERVE_SHADOW_ADS;

        //  The default policy is typically relevant when copying across
        //  zones. It might happen that the destination doesn't yet exist,
        //  and if so we try to find the previous edition in the target zone
        //  and merge its ads into our new pages (based on slot names).
        if (!shadow_pgv->layout_mime_type()) {
          //  Determine that we are copying across zones but within the same
          //  master.
          REP.Edition ed = edition();
          REP.Edition shadow_ed = shadow_pgv->edition();
          if (ed->ze_master_edition() == shadow_ed->ze_master_edition()) {
            string shadow_ze_zone_code = shadow_ed->ze_zone_code();
            string shadow_ze_edition_code = shadow_ed->ze_edition_code();
            if (shadow_ze_edition_code &&
                (ed->ze_zone_code() != shadow_ze_zone_code)) {
              //  Find best edition to copy from. We already know the target
              //  edition has a time-based edition code so find the one
              //  preceding it.
              array(REP.Edition) ze_siblings =
                filter(shadow_ed->ze_collection(),
                       lambda(REP.Edition ze) {
                         return ze->ze_zone_code() == shadow_ze_zone_code;
                       });
              sort(ze_siblings->ze_sort_key(), ze_siblings);
              int shadow_pos = search(ze_siblings, shadow_ed);
              if (shadow_pos > 0) {
                //  Do this the primitive way by mapping the first PGV slot
                REP.Edition copy_ads_ed = ze_siblings[shadow_pos - 1];
                if (REP.PageSlot first_ps = shadow_pgv->first_page_slot()) {
                  string first_ps_name = first_ps->page_name();
                  foreach (copy_ads_ed->page_slots(0), REP.PageSlot ad_ps) {
                    if (ad_ps->page_name() == first_ps_name) {
                      //  Got a match so this is the PGV we should copy from.
                      //  We can however run a simple pre-check if the Ads
                      //  layer exists and is non-empty since we otherwise
                      //  can skip this step.
                      ad_init_pgv = ad_ps->page()->pgv();
                      if (mapping doc_layout_info =
                          ad_init_pgv->get("doc_layout_info")) {
                        if (array(mapping) layers = doc_layout_info->layers) {
                          bool got_ads_layer = false;
                          foreach (layers, mapping l) {
                            //  FIXME: Hard-coded Ads layer name (like in
                            //  many other places...).
                            if ((l->name == "Ads") && !l->empty)
                              got_ads_layer = true;
                          }
                          if (!got_ads_layer)
                            ad_init_pgv = 0;
                        }
                      }
                      break;
                    }
                  }
                }
              }
            }
          }
        }
      }

      //  Find preferred slots and execute the copy
      array(REP.PageSlot) shadow_pss =
        shadow_pss_per_edition[shadow_pgv->edition()];
      Concurrent.Future/*REP.PGVersion*/ f =
        REP.IDS.shadow_copy(this, shadow_pgv, policy, ad_init_pgv, shadow_pss);
      futures += ({ f });
    }
  }

  return Concurrent.results(futures);
}


string|int(0..0) get_applauncher_url(void|bool allow_unproc,
                                     void|bool read_only)
//! Returns the URL that will open this @[REP.PGVersion] for
//! Application Launcher editing, or 0 if no layout file is available.
//!
//! @param read_only
//! If true, an URL that will let you open but not save the layout file will be
//! returned.
//!
{
  //  Verify that a layout file exists
  if (string filepath =
      get_page_filepath("layout-file", 0) ||
      (allow_unproc && get_page_filepath("unproc-layout-file", 0))) {
    return obj_table()->print_db->get_pgv_applauncher_url (this, read_only);
  }
  return 0;
}

mixed get_unver_ext (string field)
{
  ASSERT_IF_DEBUG (avail_unver_ext_fields[field]);
  switch (field) {
  case "ad_pending_updates":
    return sizeof (get_unver ("admgr_pending_jobs") || ({})) && 1;
  case "page_categories":
    return categories();
  default:
  }

  return get_unver (field);
}

mapping(string:mixed) get_unver_client_rec()
{
  if (!pg_volatile->unver_fields_cache)
    pg_volatile->unver_fields_cache = ([]);

  mapping(string:mixed) res = pg_volatile->unver_fields_cache + ([]);
  foreach (avail_unver_ext_fields - res; string field;) {
    mixed val = get_unver_ext (field);
    if (!zero_type (val)) {
      res[field] = val;
      if (cacheable_unver_fields[field]) {
        pg_volatile->unver_fields_cache[field] = val;
      }
    }
  }

  return res;
}

int(0..1) package_file_downloadable()
//! Returns 1 if the package_file_status_globs setting in the print-db
//! module allows the REP.Storage.PACKAGE_FILE for the page status
//! that this PGVersion is currently in, and the license key permits
//! package file downloads. Note: this doesn't mean that the package
//! file exists.
{
  //  We need a layout file to even consider packages
  if (!get_existing_page_filepath(LAYOUT_FILE))
    return 0;

  if (!REP.License.feature_enabled (REP.License.ind_package_file))
    return 0;

  array(string) package_file_status_globs = REP.get_print_module()
    ->query ("package_file_status_globs");

  string my_status = page_status();

  foreach (package_file_status_globs, string gl)
    if (glob (gl, my_status)) return 1;

  return 0;
}

int pgv_storage_in_use (int storage_num)
//! Returns true if this pgv references any file that is stored in the
//! pgv storage directory with the given number. The storage number is
//! assumed to be in the same page group.
{
  ASSERT_IF_DEBUG (storage_num);

  if (rec->storage_counter == storage_num)
    return 1;

  // Check externally stored text flow links.
  if (rec->text_flow_links)
    foreach (rec->text_flow_links; string uuid; mapping(string:mixed) fields)
      foreach (REP.TextFlowLink.ext_fields; string ext_field;)
	if (fields[ext_field + "_ext"] == storage_num)
	  return 1;

  return 0;
}

int same_pgv_storage(REP.PGVersion other_pgv)
//! Returns true of this object shares storage with another version. This
//! means the layout file is identical even if other metadata such as status
//! have changed.
{
  return
    other_pgv &&
    (rec->storage_counter == other_pgv->get("storage_counter"));
}



// Story slot handling

#if 0
/* protected */ void internal_story_slots_changed()
{
  dirty_fields->story_slots = 1;
  if (!auto_commit_disabled_count && id())
    commit();
}

protected mapping(string:StorySlot) wrapped_story_slots;

protected void wrap_story_slots()
{
  mapping(string:StorySlot) wrapped = ([]);
  if (rec->story_slots) {
    foreach (rec->story_slots; string idx; mapping(string:mixed) data) {
      StorySlot slot = StorySlot (data, this);
      wrapped[slot->get_slot_id()] = slot;
    }
    if (!wrapped_story_slots)
      wrapped_story_slots = wrapped;
  } else {
    rec->story_slots = ([]);
    wrapped_story_slots = ([]);
  }
}

array(StorySlot) story_slots()
{
  if (!wrapped_story_slots)
    wrap_story_slots();

  array(StorySlot) res = values (wrapped_story_slots);
  sort (res->get_display_name(), res);
  return res;
}

void set_story_slots (array(StorySlot) slots)
{
  mapping(string:mapping) new_slot_data = ([]);
  mapping(string:StorySlot) new_slots = ([]);

  foreach (slots, REP.PGVersion.StorySlot slot) {
    new_slot_data[slot->get_slot_id()] = slot->data;
    new_slots[slot->get_slot_id()] = slot;
  }

  rec->story_slots = new_slot_data;
  wrapped_story_slots = new_slots;
  internal_story_slots_changed();
}

string get_story_slot_id (int(0..0)|string slot_class,
			  int(0..0)|string slot_name,
			  int sequence)
{
  string prefix = slot_class ? "c" : "n";
  string id_string = slot_class || slot_name;
  if (!id_string || !sequence) return 0;
  return sprintf ("%d:%s:%s#%d", id(), prefix, id_string, sequence);
}

StorySlot get_story_slot (int(0..0)|string slot_class,
			  int(0..0)|string slot_name,
			  int sequence,
			  void|int only_existing)
{
  if (!wrapped_story_slots)
    wrap_story_slots();

  string id = get_story_slot_id (slot_class, slot_name, sequence);

  if (!id) return 0;

  if (StorySlot slot = wrapped_story_slots[id])
    return slot;
  else if (only_existing)
    return 0;

  mapping(string:mixed) data = ([]);

  if (slot_class) data->slot_class = slot_class;
  if (slot_name) data->slot_name = slot_name;
  if (sequence) data->slot_sequence = sequence;

  StorySlot slot = StorySlot (data, this);

  if (!wrapped_story_slots[id]) {
    wrapped_story_slots[id] = slot;
    rec->story_slots[id] = data;
    internal_story_slots_changed();
  }

  return wrapped_story_slots[id];
}

StorySlot story_slot_by_class (string story_class, int sequence)
{
  return get_story_slot (story_class, 0, sequence);
}

StorySlot story_slot_by_name (string name, int sequence)
{
  return get_story_slot (0, name, sequence);
}

StorySlot story_slot_by_id (string id, void|int only_existing,
			    void|REP.OnError on_invalid_id)
{
  if (sscanf (id, "%d:%s:%s#%d", int pgv_id, string type, string spec,
	      int sequence) == 4) {
    ASSERT_IF_DEBUG (pgv_id == this_program::id());
    string story_class;
    string story_name;
    if (type == "c") story_class = spec;
    else if (type == "n") story_name = spec;

    if (story_class || story_name)
      return get_story_slot (story_class, story_name, sequence, only_existing);
  }

  return REP.raise_err (on_invalid_id, "Invalid story slot identifier %s.\n",
			id);
}

class StorySlot
//! Represents a story slot in the PGVersion. A story slot can either
//! be identified by a slot class or a slot name. A slot name
//! identifies a specific StoryTemplateDef, while a slot class
//! identifies any StoryTemplateDef of that class. In both cases, a
//! sequence number points out a specific location between multiple
//! instances of the class/name.
{
  /* protected */ mapping(string:mixed) data; // Internal.
  REP.PGVersion pgv;
  int|REP.Story placed_story = -1;

  protected void int_upd()
  {
    pgv->internal_story_slots_changed();
  }

  string get_slot_id()
  {
    return pgv->get_story_slot_id (data->slot_class, data->slot_name,
				   data->slot_sequence);
  }

  string get_ids_name_spec()
  {
    // FIXME: The InDesign stuff doesn't support placement in classes
    // right now.
    ASSERT_IF_DEBUG (data->slot_name);
    return sprintf ("%s#%d", data->slot_name, data->slot_sequence);
  }

  string get_display_name()
  {
    return (string)data->slot_sequence;
  }

  void set_placed_story (int(0..0)|REP.Story story)
  {
    if (story) {
      REP.DB.set_story_app_fields (
	(["story": story,
	  "pgv": pgv,
	  "is_placed": 1,
	  "story_slot": this]));
    } else {
      if (REP.Story cur_story = get_placed_story()) {
	REP.DB.set_story_app_fields (
	  (["story": story,
	    "pgv": pgv,
	    "story_slot": Val.null]));
      }
    }
    placed_story = story;
  }

  void place_story (REP.Story story)
  {
    REP.get_print_module()->call_layout_engine ("place_story_on_page",
						story, this);
    placed_story = story;
  }

#if 0
  void unplace_story()
  {
    if (REP.Story story = get_placed_story()) {
      REP.get_print_module()->call_layout_engine ("unplace_story_on_page",
						  story, this);
    }
    placed_story = 0;
  }
#endif

  int(0..0)|REP.Story get_placed_story()
  {
    if (placed_story == -1) {
      REP.CachedTable sa_tbl = REP.DB.story_apps_table();
      mapping(string:mixed) row =
	sa_tbl->select ("story_app_parts.story_slot = '" +
			sa_tbl->quote (get_slot_id()) + "'",
			0,
			({ "story_app_parts.page_group_id",
			   "story_apps.story_id" }),
			"JOIN story_app_parts USING (story_app_id)")->
	fetch();
      if (row) placed_story = REP.DB.story_by_id (row->story_id,
						  REP.RETURN_ZERO);
      else placed_story = 0;
    }

    return placed_story;
  }

  protected void create (mapping(string:mixed) data, REP.PGVersion pgv)
  {
    ASSERT_IF_DEBUG (data->slot_class || data->slot_name);
    ASSERT_IF_DEBUG (data->slot_sequence);

    // Mutually exclusive.
    ASSERT_IF_DEBUG (!data->slot_class || !data->slot_name);

    this_program::data = data;
    this_program::pgv = pgv;
  }
}
#endif

//! Updates the specified stories on page, or all placed stories if
//! @[stories_to_update] is unspecified. Note that this does not
//! reflow.
void update_stories_on_page (void|array(REP.Story) stories_to_update,
			     void|function(int,REP.PGVersion:void) done_cb)
{
  if (!stories_to_update) stories_to_update = stories(0, 1, 1);
  REP.get_print_module()->call_layout_engine ("update_story_on_page",
					      stories_to_update, this,
					      done_cb);
}


// Text chain handling

#ifdef TEXT_CHAIN_DEBUG
#define TEXT_CHAIN_MSG(X...) werror (X)
#else
#define TEXT_CHAIN_MSG(X...) 0
#endif

private inherit Thread.Mutex: rec_tfl_mutex;
// A mutex to make rec->text_flow_links updates atomic. Necessary
// since the links must be kept unique both wrt uuid and doc_id, and
// we can't fix that with the interpreter lock stunt.
//
// Note: Does not cover the updates in pg_volatile (doesn't have the
// right scope for that).

protected mapping(int:mapping(string:mixed)) tflf_by_doc_id;
// Cached lookup from doc_id to the text flow link field mappings.
// This cache is specific for this pgv, so the values aren't
// TextFlowLink objects.

protected void unlocked_build_tfl_lookups (
  mapping(string:REP.TextFlowLink) tfl_by_uuid)
// Assumes rec_tfl_mutex. Must be called from the current pgv if
// tfl_by_uuid is set.
{
  ASSERT_IF_DEBUG (!tfl_by_uuid || !sizeof (tfl_by_uuid));
  tflf_by_doc_id = ([]);

  if (rec->text_flow_links) {
    foreach (rec->text_flow_links; string uuid; mapping(string:mixed) fields) {
      ASSERT_IF_DEBUG (uuid/*%O*/ == fields/*%O*/->uuid, uuid, fields);

      if (tfl_by_uuid)
	tfl_by_uuid[uuid] = REP.TextFlowLink (fields, this);

      if (int doc_id = fields->doc_id) {
	ASSERT_IF_DEBUG (zero_type (tflf_by_doc_id/*%O*/[doc_id])
			 /*fields: %O*/, tflf_by_doc_id, fields);
	tflf_by_doc_id[doc_id] = fields;
      }
    }
  }

  int pgv_id = id();

  if (tfl_by_uuid) {
    // vvv Relying on the interpreter lock from here.
    pg_volatile->tfl_pgv_id = pgv_id;
    pg_volatile->tfl_by_uuid = tfl_by_uuid;
    // ^^^ Relying on the interpreter lock to here.
  }
}

mapping(string:REP.TextFlowLink) cur_text_flow_links()
//! Returns a mapping containing the current @[TextFlowLink]s for this
//! page group, indexed by uuid. Don't be destructive on the returned
//! value. Doesn't return zero.
{
  int tfl_pgv_id = pg_volatile->tfl_pgv_id;
  // Relying on the interpreter lock here.
  mapping(string:REP.TextFlowLink) tfl_by_uuid = pg_volatile->tfl_by_uuid;

  if (REP.PGVersion cur = current_pgv()) {
    if (tfl_pgv_id == cur->id())
      return tfl_by_uuid;

    if (cur != this)
      return cur->cur_text_flow_links();
  }

  Thread.MutexKey lock = rec_tfl_mutex::lock();
  tfl_by_uuid = ([]);
  unlocked_build_tfl_lookups (tfl_by_uuid);
  destruct (lock);

  return tfl_by_uuid;
}

REP.TextFlowLink text_flow_link_by_uuid (string uuid)
//! Returns the current (or last known from deconstruct)
//! @[TextFlowLink] for this page group with the given @[uuid], if
//! any.
{
  return cur_text_flow_links()[uuid];
}

mapping(string:mixed) versioned_tfl_by_doc_id (int doc_id)
//! Returns the field mapping for the text flow link in this pgv with
//! the given @[doc_id], if any. Don't be destructive on the returned
//! value.
//!
//! Since this function returns data specific to this version, it does
//! not return a @[TextFlowLink].
{
  if (!tflf_by_doc_id) {
    Thread.MutexKey lock = rec_tfl_mutex::lock();
    if (current_pgv() == this)
      // Can build pg_volatile->tfl_by_uuid at the same time.
      unlocked_build_tfl_lookups (([]));
    else
      unlocked_build_tfl_lookups (0);
    destruct (lock);
  }

  return tflf_by_doc_id[doc_id];
}

mapping(string:mixed) versioned_tfl_by_uuid (string uuid)
//! Returns the field mapping for the text flow link in this pgv with
//! the given @[uuid], if any. Don't be destructive on the returned
//! value.
//!
//! Since this function returns data specific to this version, it does
//! not return a @[TextFlowLink].
{
  if (mapping(string:mapping(string:mixed)) links = rec->text_flow_links)
    return links[uuid];
  return UNDEFINED;
}

mixed versioned_tfl_field (string uuid, string field)
//! Returns the value of a field in a text flow link stored in this pgv.
//!
//! @note
//! Snippets are returned as octet strings, i.e. without decoding
//! any charset.
{
  ASSERT_IF_DEBUG (REP.TextFlowLink.avail_fields[field]);
  if (mapping(string:mixed) fields = versioned_tfl_by_uuid (uuid)) {
    if (REP.TextFlowLink.ext_fields[field]) {
      if (int id = fields[field + "_ext"])
	if (string path = get_ext_tfl_field_path (uuid, field, id))
	  if (string res = REP.Storage.read_db_file (path, 1))
	    return res;
    }
    else
      return fields[field];
  }
  return UNDEFINED;
}

string versioned_tfl_path (string uuid, string field)
//! Like @[versioned_tfl_field], but returns the path to the file that
//! is used to store the value of an external text flow link field.
//! The path is relative to the REP file DB root.
//!
//! @returns
//! Returns zero if the value isn't set at all or is stored internally.
{
  ASSERT_IF_DEBUG (REP.TextFlowLink.avail_fields[field]);
  if (mapping(string:mixed) fields = versioned_tfl_by_uuid (uuid))
    if (REP.TextFlowLink.ext_fields[field])
      if (int id = fields[field + "_ext"])
	return get_ext_tfl_field_path (uuid, field, id);
  return 0;
}

int|Stdio.Stream versioned_tfl_data_stream (string uuid, string field,
					    void|REP.OnError on_io_err)
//! Like @[versioned_tfl_field] but returns the data from this field
//! as a file object opened for reading.
//!
//! @param uuid
//! The uuid of the text flow link to query.
//!
//! @param field
//! The field to retrieve.
//!
//! @param on_io_err
//! How to react if there is an I/O error opening the file.
//!
//! @returns
//! Returns a file opened for reading at the beginning (or an
//! @[Stdio.FakeFile] if the value is internal). Returns zero if the
//! field has no value, or has an internal nonstring value.
//!
//! @note
//! Snippets are returned as octet strings, i.e. without decoding
//! any charset.
{
  ASSERT_IF_DEBUG (REP.TextFlowLink.avail_fields[field]);
  if (mapping(string:mixed) fields = versioned_tfl_by_uuid (uuid)) {
    if (REP.TextFlowLink.ext_fields[field]) {
      if (int id = fields[field + "_ext"])
	if (string path = get_ext_tfl_field_path (uuid, field, id))
	  if (Stdio.Stream res =
	      REP.Storage.read_open_db_file (path, 1, on_io_err))
	    return res;
    }
    else {
      mixed val = fields[field];
      if (stringp (val))
	return Stdio.FakeFile (val);
    }
  }
  return 0;
}

void set_text_flow_link (mapping(string:mixed) fields)
//! Adds or changes a text flow link entry in the text_flow_links
//! property. An existing entry with the same uuid is changed. Fields
//! are removed from it by specifying @[Val.null] as value.
//!
//! If @[fields] has a "doc_id" entry and there's another text link
//! entry with the same doc_id, then that other one is removed.
//!
//! The externally stored fields can be set either through strings,
//! file objects open for reading, or by specifying a path. A path is
//! given in an entry named "<field>_path". It should be absolute, and
//! the file there is hardlinked/copied through
//! @[REP.Storage.copy_db_file].
//!
//! This function is destructive on @[fields].
//!
//! @note
//! The snippet fields are preferably set through file storage
//! functions like @[store_file]. Use the field names (which are
//! identical to @[REP.Storage.HEAD_SNIPPET] etc) as file type and the
//! uuid as subspec.
{
  string uuid = m_delete (fields, "uuid");
  ASSERT_IF_DEBUG (stringp (uuid));

  foreach (REP.TextFlowLink.ext_fields; string ext_field;) {
    if (string path = m_delete (fields, ext_field + "_path")) {
      ASSERT_IF_DEBUG (zero_type (fields[ext_field/*%O*/]), ext_field);
      if (store_file_by_copy (path, ext_field, uuid, 1, REP.LOG_ERROR))
	fields[ext_field + "_ext"] = rec->storage_counter;
    }
    else {
      string|Stdio.Stream|Val.Null val = m_delete (fields, ext_field);
      if (!zero_type (val)) {
	if (val == Val.null)
	  fields[ext_field + "_ext"] = Val.null;
	else {
	  if (store_file (val, ext_field, uuid, REP.LOG_ERROR))
	    fields[ext_field + "_ext"] = rec->storage_counter;
	}
      }
    }
  }

  int changed, chain_change;
  int changed_doc_id;
  Thread.MutexKey lock = rec_tfl_mutex::lock();

  mapping(string:mapping(string:mixed)) tfl_val = rec->text_flow_links;
  if (!tfl_val) {
    tfl_val = rec->text_flow_links = ([]);
    changed = 1;
  }

  mapping(string:mixed) link_rec = tfl_val[uuid];
  if (!link_rec) {
    link_rec = tfl_val[uuid] = (["uuid": uuid]);
    changed = chain_change = 1;
  }

  foreach (fields; string field; mixed val) {
    if (val == Val.null) {
      if (!zero_type (m_delete (link_rec, field)))
	changed = 1;
    }
    else if (!equal (val, link_rec[field])) {
      link_rec[field] = val; // Let's skip copy_value here.
      changed = 1;
      if (field == "doc_id") {
	ASSERT_IF_DEBUG (intp (val) && val /*%O*/, val);
	changed_doc_id = val; // Need to look for duplicate link to remove.
      }
    }
  }

  if (changed_doc_id) {
    string dup_uuid;
    if (mapping(int:mapping(string:mixed)) lookup = tflf_by_doc_id) {
      if (mapping(string:mixed) link_fields = lookup[changed_doc_id])
	if (link_fields->uuid != uuid)
	  dup_uuid = link_fields->uuid;
    }
    else			// Got no lookup cache.
      foreach (tfl_val; string u; mapping(string:mixed) other_rec)
	if ((u != uuid) && (other_rec->doc_id == changed_doc_id)) {
	  dup_uuid = u;
	  break;
	}

    if (dup_uuid) {
      m_delete (tfl_val, dup_uuid);
      chain_change = 1;
      ASSERT_IF_DEBUG (changed);
    }
  }

  // Note: Duplicate invalidation code in remove_text_flow_link and
  // update_shared_storage_fields.

  if (chain_change || changed_doc_id) {
    // Lookup caches are stale only when links are added or removed
    // or if the doc_id changes.
    tflf_by_doc_id = 0;
    if (current_pgv() == this) {
      // The pg_volatile->tfl_pgv_id check in cur_text_flow_links avoids
      // using stale caches in all cases except when changing the
      // current pgv, so we have to zap the caches now.
      // vvv Relying on the interpreter lock from here.
      m_delete (pg_volatile, "tfl_pgv_id");
      m_delete (pg_volatile, "tfl_by_uuid");
      // ^^^ Relying on the interpreter lock to here.
    }
  }

  destruct (lock);

  if (chain_change)
    edition()->invalidate_text_chains();

  if (changed) {
    dirty_fields->text_flow_links = 1;
    if (!auto_commit_disabled_count)
      commit();
  }
}

void remove_text_flow_link (string uuid)
//! Removes the text flow link entry with the given uuid, if any.
{
  Thread.MutexKey lock = rec_tfl_mutex::lock();
  mapping(string:mapping(string:mixed)) tfl_val = rec->text_flow_links;

  if (tfl_val && m_delete (tfl_val, uuid)) {
    // Invalidate like in set_text_flow_link.

    tflf_by_doc_id = 0;
    if (current_pgv() == this) {
      // vvv Relying on the interpreter lock from here.
      m_delete (pg_volatile, "tfl_pgv_id");
      m_delete (pg_volatile, "tfl_by_uuid");
      // ^^^ Relying on the interpreter lock to here.
    }

    destruct (lock);

    edition()->invalidate_text_chains();

    dirty_fields->text_flow_links = 1;
    if (!auto_commit_disabled_count)
      commit();
  }
}

int got_up_to_date_snippet (string uuid, REP.Storage.FileType snippet_type)
//! Returns true if the snippet of the specified type
//! (@[REP.Storage.HEAD_SNIPPET] or @[REP.Storage.OVERFLOW_SNIPPET])
//! stored for the text chain @[uuid] in this pgv exists and is
//! up-to-date wrt the current version of the page group. Doesn't
//! check existence on disk though.
{
  ASSERT_IF_DEBUG (REP.TextFlowLink.ext_fields[snippet_type]);
  if (REP.PGVersion cur_pgv = current_pgv())
    if (cur_pgv->get ("storage_counter") == rec->storage_counter)
      if (mapping(string:mixed) cur_fields =
	  cur_pgv->versioned_tfl_by_uuid (uuid)) {
	int id = cur_fields[snippet_type + "_ext"];
	if (snippet_type == REP.Storage.OVERFLOW_SNIPPET) {
	  if (!id)
	    // A missing overflow snippet implies there's no overflow, so it's
	    // up-to-date.
	    return 1;
	}
	else {
	  if (!id)
	    // A missing head snippet is up-to-date if we shouldn't have one.
	    // Happens in practice when a chain has been split and there are
	    // pieces in which no link has the head text.
	    return !cur_fields->has_head_text;
	}
	return id == rec->storage_counter;
      }
  return 0;
}

int pg_needs_reflow (void|string text_chain_uuid)
//! Returns nonzero if this page group has a text chain with the given
//! uuid that needs reflow: 1 if text needs to be updated, 2 if it
//! needs to be sent to reflow_text_chains_on_page to extract snippets
//! that are needed for other pages, 3 if both.
//!
//! Checks all text chains in the page group if @[text_chain_uuid] is
//! left out.
//!
//! This function checks all the links back to the start of the chain.
{
  mapping(string:REP.TextFlowLink) links = cur_text_flow_links();

  if (text_chain_uuid) {
    if (REP.TextFlowLink link = links[text_chain_uuid])
      links = ([text_chain_uuid: link]);
    else
      return 0;
  }

  int res;
  mapping(string:array(REP.TextFlowLink)) text_chains =
    edition()->get_text_chains();

  foreach (links; string uuid; REP.TextFlowLink link) {
    if (int update_type = link->reflow_source()[0])
      res |= update_type;
    else if (array(REP.TextFlowLink) chain = text_chains[uuid]) {
      foreach (chain, REP.TextFlowLink other_link) {
	if (other_link == link) break;
	if (other_link->reflow_source()[0] == 1) {
	  res |= 1;
	  break;
	}
      }
    }
  }

  return res;
}

void reflow_page (void|multiset(string) text_chain_uuids,
		  void|function(int,REP.PGVersion:void) done_cb)
//! Invokes the layout server to reflow the given text chains in this
//! page group, if necessary. Reflows all text chains in the page
//! group if @[text_chain_uuids] is left out.
//!
//! If snippets have to be extracted from other page groups first then
//! that is done, but it does not transitively reflow text chains in
//! earlier page groups. Such chains are skipped instead.
{
  mapping(string:REP.TextFlowLink) links = cur_text_flow_links();

  mapping(REP.PGVersion:multiset(string)) extract_pgvs = ([]);
  multiset(string) update_chains = (<>);

  foreach (text_chain_uuids || links; string uuid;) {
    REP.TextFlowLink link = links[uuid];
    [int update_type, REP.PGVersion src_pgv,
     REP.Storage.FileType src_snippet] = link->reflow_source();
    if (update_type == 1) {
      int src_needs_reflow = src_pgv->pg_needs_reflow (uuid);
      if (src_snippet != REP.Storage.HEAD_SNIPPET &&
	  src_needs_reflow & 1) {
	// Skip this chain if the source must be reflowed first. But
	// ignore the reflow flag if we want the head snippet - must
	// extract it even if the old head link needs a reflow later.
      }
      else {
	update_chains[uuid] = 1;
	if (src_needs_reflow & 2) {
	  if (!extract_pgvs[src_pgv])
	    extract_pgvs[src_pgv] = (<uuid>);
	  else
	    extract_pgvs[src_pgv][uuid] = 1;
	}
      }
    }
  }

  TEXT_CHAIN_MSG ("%O->reflow_page (%O): %O %O\n",
		  this, text_chain_uuids, extract_pgvs, update_chains);

  if (!sizeof (update_chains)) {
    if (done_cb) done_cb (0, 0);
    return;
  }

  if (int extract_count = sizeof (extract_pgvs)) {
    void extract_done_cb (int status) {
      if (status < 0) {
	if (extract_count > 0) {
	  if (done_cb) done_cb (status, 0);
	  extract_count = 0;
	}
      }
      else {
	if (!--extract_count)
	  REP.get_print_module()->call_layout_engine (
	    "reflow_text_chains_on_page", this, update_chains, done_cb);
      }
    };
    foreach (extract_pgvs; REP.PGVersion pgv; multiset(string) uuids)
      REP.get_print_module()->call_layout_engine (
	"reflow_text_chains_on_page", pgv, uuids, extract_done_cb);
  }
  else
    REP.get_print_module()->call_layout_engine (
      "reflow_text_chains_on_page", this, update_chains, done_cb);
}

string describe_preflight_results()
//! Formats preflight information as text suitable for
//! client side representation.
{
  mapping pf_res = (rec->preflight_results || ([ ]) ) + ([ ]);

  //  Check for jump problems
  mapping(string:int(1..1)) jump_entry = ([ ]);
  if (mapping(string:int(1..1)) jump_preflight =
      REP.DB.pgv_jump_preflight(this)) {
    if (jump_preflight["jump-head"])
      jump_entry["Relinking needed"] = 1;
    if (jump_preflight["jump-reflow"])
      jump_entry["Reflow needed"] = 1;
    if (jump_preflight["jump-xref"])
      jump_entry["Cross-ref update needed"] = 1;
  }
  if (sizeof(jump_entry))
    pf_res += ([ "JUMPS": jump_entry ]);

  if (!sizeof(pf_res))
    return 0;

  string pf_txt = "";
  foreach (sort(indices(pf_res)), string pf_key) {
    array|mapping pf_entry = pf_res[pf_key];
    sscanf(pf_key, "%s (%*d)", pf_key);

    array(string) descs;
    if (mappingp (pf_entry)) {
      descs = indices (pf_entry);

      //  Special handling of LINKS and TEXT keys so we can report more
      //  details to the user.
      if (pf_key == "LINKS") {
        foreach (descs; int idx; string desc) {
          if (has_prefix(lower_case(desc), "missing link (")) {
            array(string) links =
              sort(Array.uniq(column(pf_entry[desc], 1)));
            descs[idx] =
              sprintf("Missing link (%s)", links * ", ");
          }
        }
      } else if (pf_key == "TEXT") {
        foreach (descs; int idx; string desc) {
          if (has_prefix(lower_case(desc), "missing font (")) {
            array(string) fonts = sort(indices(pf_entry[desc]));
            foreach (fonts; int idx2; string font) {
              string font2;
              sscanf(reverse(font), ")%*d( %s", font2);
              if (font2)
                fonts[idx2] = String.trim_all_whites(reverse(font2));
            }
            descs[idx] =
              sprintf("Missing font (%s)", fonts * ", ");
          }
        }
      }
    } else if (arrayp (pf_entry)) {
      descs = ({});
      foreach (pf_entry, array a)
        descs += ({ a[1] });
    } else {
      werror ("Unknown pf_entry %O\n", pf_entry);
      continue;
    }
    if (sizeof(pf_txt))
      pf_txt += "\n";

    //  Note that the Page Close-Up panel splits on the bullet string
    //  ("\x2022 ") to present individual warnings. It also requires the
    //  "KEY: Data" format for all items.
    pf_txt +=
      "\x2022 " + pf_key + ": " +
      (sort(descs) * ", ");
    if (!has_suffix(pf_txt, "."))
      pf_txt += ".";
  }

  return Roxen.encode_xml_invalids(pf_txt);
}

void set_page_group_category(string|int(0..0) main_category)
{
  //  Delete existing categories (both main and secondary) for page
  int pg_id = rec->page_group_id;
  SqlTools.SqlTable pc_tbl = REP.DB.page_categories_table();
  pc_tbl->delete("page_group_id = " + pg_id);

  //  Set new category
  if (main_category) {
    pc_tbl->insert_or_update(([ "page_group_id" : pg_id,
                                "page_category" : main_category,
                                "is_main"       : 1,
                                "is_inherited"  : 0 ]));
  }

  // Clear unver fields property cache
  if (pg_volatile->unver_fields_cache)
    m_delete (pg_volatile->unver_fields_cache, "page_categories");

  // Notify edition about slot update. A potential improvement could
  // be to verify that categories actually did change.
  array(REP.PageSlot) slots = pages()->page_slot() - ({ 0 });
  REP.Edition edition = edition();
  REP.Notification.Client.edition_content_update(edition, sizeof(slots) && slots);
}


mapping(string:string)
get_story_notes_and_page_comments(array(string) note_field_patterns,
				  int(0..1) use_ext_page_id)
//!  Returns a mapping from external page ID (as string) to a HTML-formatted
//!  block of comments containing both page description and individual story
//!  note fields. These HTML chunks can be rendered by e.g. setPageComments()
//!  in roxen.js.
//!
//!  The @[note_field_patterns] array may list all story fields potentially
//!  holding notes. It may hold entries like "component/field" or "*/field".
//!  For example:
//!
//!    ({ "main/article-notes",
//!       "*/messages",
//!       ...
//!    })
//!
//!  If @[note_field_patterns] is empty only page description comments are
//!  returned.
{
  mapping(int:mapping(string:mapping)) story_notes = ([ ]);
  if (note_field_patterns && sizeof(note_field_patterns)) {
    //  We identify story candidates two ways:
    //
    //   - Those placed on the page
    //   - Those planned for this page group
    //
    //  The second type will be listed on every page in the group since
    //  we don't have info to narrow down to a specific page.
    array(REP.Story) pgv_stories = stories(0, 0, 0);
    mapping(REP.Story:int(1..1)|multiset(int)) story_queue = ([ ]);
    foreach (pgv_stories, REP.Story s) {
      //  Place marker that we want to target all pages
      story_queue[s] = 1;
    }

    //  For placements we tune the page IDs that we need to update
    foreach (placements(), REP.PagePlacement p) {
      if (p->is_rep_story_placement) {
	if (REP.Story story = p->story()) {
	  int page_id = p->page()->id();
          int(1..1)|multiset(int) queue_entry = story_queue[story];
          if (queue_entry == 1) {
            story_queue[story] = (< page_id >);
          } else if (multisetp(queue_entry)) {
            story_queue[story][page_id] = 1;
          }
        }
      }
    }

    //  Now loop through queue
    array(int) pgv_pg_ids = pages()->id();
    foreach (story_queue; REP.Story story; int(1..1)|multiset(int) page_ids) {
      //  The name of the notes field is not necessarily the same across
      //  all story structures, so aggregate all the field references we
      //  can resolve.
      array(REP.SIVersion) cur_items = story->current_items();
      foreach (cur_items, REP.SIVersion siv) {
        //  Check if component name matches first segment of the field
        //  pattern list.
        REP.ComponentDef comp_def = siv->component_def();
        string comp_name = comp_def->get_base_name();
        foreach (note_field_patterns, string note_field_pattern) {
          array(string) pattern_segs = note_field_pattern / "/";
          if (sizeof(pattern_segs) == 2) {
            [string note_comp, string note_field] = pattern_segs;
            if (!sizeof(note_field))
              continue;
            if (note_comp == comp_name || note_comp == "*") {
              //  Fetch field value
              REP.DataField df =
                siv->data_field_by_name(note_field, REP.RETURN_ZERO);
              mixed data = df && df->get_data();
              if (stringp(data) && sizeof(data)) {
                //  Make sure it's formatted as plain-text-multiline
                data =
                  REP.LDE.convert_value_to_type(data, df->field_def(),
                                                "plain-text-multiline");

                //  Expand plain-text-multiline to HTML paragraphs with
                //  line-breaks.
                string data_html =
                  "<p class='note-item-indent'>" +
                  replace(Roxen.html_encode_string(data),
                          "\n\n",
                          "</p><p class='note-item-indent'>") +
                  "</p>";
                data_html = replace(data_html, "\n", "<br/>");

                //  Substitute placeholder title if currently empty
                string story_title = story->get("title");
                string story_title_suffix = story->get("title_suffix");
                if (!sizeof(story_title || ""))
                  story_title = "(Untitled story #" + story->id() + ")";

                //  Associate to page ID and append all note fields within
                //  the same story.
                foreach (intp(page_ids) ? pgv_pg_ids : indices(page_ids),
                         int page_id) {
                  if (!story_notes[page_id])
                    story_notes[page_id] = ([ ]);
                  if (!story_notes[page_id][story_title]) {
                    story_notes[page_id][story_title] = ([
                      "suffix": story_title_suffix || "",
                      "html": ""
                    ]);
                  }
                  story_notes[page_id][story_title]->html += data_html;
                }
              }
            }
          }
        }
      }
    }
  }

  //  Page comments
  mapping pg_descs = ([ ]);
  foreach (pages(), REP.Page page) {
    //  Aggregate all story notes for this page
    string story_notes_for_page = "";
    if (mapping(string:mapping) notes = story_notes[page->id()]) {
      //  Sort on title
      array(string) sorted_titles = sort(indices(notes));
      foreach (sorted_titles, string title) {
        mapping data = notes[title];
        string suffix_attr =
          data->suffix ?
          (" data-suffix='" + Roxen.html_encode_string(data->suffix) + "'") :
          "";
	story_notes_for_page +=
	  "<p class='note-item-title' " + suffix_attr + ">" +
	  Roxen.html_encode_string(title) + "</p>" +
	  data->html;
      }
    }
    if (sizeof(story_notes_for_page))
      story_notes_for_page =
	"<p class='note-heading'>STORY NOTES</p>" +
	story_notes_for_page;

    //  Page description
    string pg_desc =
      String.trim_all_whites(page->get("description") || "");
    string res_page_id =
      (string) (use_ext_page_id ? page->get("ext_page_id") : page->id());

    //  Total description field
    string total_pg_desc =
      (sizeof(pg_desc) ?
       ("<p class='note-heading'>PAGE NOTES</p>" +
	"<p class='note-item'>" + Roxen.html_encode_string(pg_desc) + "</p>") :
       "") +
      story_notes_for_page;
    if (total_pg_desc && sizeof(total_pg_desc))
      pg_descs[res_page_id] = total_pg_desc;
  }

  return pg_descs;
}


// Placements code below.

REP.Bucket placements_bucket()
{
  return internal_get_bucket ("placements_bucket_id");
}

array(REP.PagePlacement) placements()
{
  return placements_bucket()->bucket_items();
}

string unver_uuid()
{
  string uuid = [string] get_unver("page_group_uuid");
  if (uuid) {
    return uuid;
  }
  error("Failed to get page group uuid for page group version.");
}
