#include <QList>

#include <bb/pim/account/Result>
#include <bb/pim/account/Account>
#include <bb/pim/account/Provider>
#include <bb/pim/account/AccountService>

#include "bb/pim/unified/unified_data_source.h"

extern "C" {

/* This code is not threadsafe -- only invoke the hub from one thread */
static uds_context_t handle = NULL;
static const char *owner = NULL;
static const char *assets = NULL;

static int reinit() {
	if(handle) uds_close(&handle);
	handle = NULL;

	if(uds_init(&handle, false) != UDS_SUCCESS) {
		handle = NULL;
		return -1;
	} else {
		int r = UDS_ERROR_TIMEOUT;
		while(r == UDS_ERROR_TIMEOUT) {
			r = uds_register_client(handle, owner, "", assets);
			if(r == UDS_ERROR_DISCONNECTED) reinit();
		}

		if(r == UDS_SUCCESS) return 0;
		return -2;
	}
}

/* XXX: We don't dup the strings for now, so be sure to keep them around! */
Q_DECL_EXPORT int hub_init(const char *_owner, const char *_assets) {
	owner = _owner;
	assets = _assets;
	return reinit();
}

Q_DECL_EXPORT int hub_update_account(uds_account_data_t *account) {
	if(!handle) reinit();
	if(!handle) return -1;

	int r = UDS_ERROR_TIMEOUT;

	while(r == UDS_ERROR_TIMEOUT) {
		r = uds_account_updated(handle, account);
		if(r == UDS_ERROR_DISCONNECTED) {
			reinit();
			if(!handle) return -1;
		}
	}

	if(r == UDS_SUCCESS) return 0;

	r = UDS_ERROR_TIMEOUT;
	while(r == UDS_ERROR_TIMEOUT) {
		r = uds_account_added(handle, account);
		if(r == UDS_ERROR_DISCONNECTED) {
			reinit();
			if(!handle) return -1;
		}
	}

	if(r == UDS_SUCCESS) return 0;
	return -2;
}

Q_DECL_EXPORT int hub_remove_account(long long int accountId) {
	if(!handle) reinit();
	if(!handle) return -1;

	int r = UDS_ERROR_TIMEOUT;
	while(r == UDS_ERROR_TIMEOUT) {
		r = uds_account_removed(handle, accountId);
		if(r == UDS_ERROR_DISCONNECTED) {
			reinit();
			if(!handle) return -1;
		}
	}

	if(r != UDS_SUCCESS) return -2;
	bb::pim::account::AccountService service;
	service.deleteAccount(accountId);

	return 0;
}

Q_DECL_EXPORT int hub_update_item(uds_inbox_item_data_t *item) {
	if(!handle) reinit();
	if(!handle) return -1;

	int r = UDS_ERROR_TIMEOUT;

	while(r == UDS_ERROR_TIMEOUT) {
		r = uds_item_updated(handle, item);
		if(r == UDS_ERROR_DISCONNECTED) {
			reinit();
			if(!handle) return -1;
		}
	}

	if(r == UDS_SUCCESS) return 0;

	r = UDS_ERROR_TIMEOUT;
	while(r == UDS_ERROR_TIMEOUT) {
		r = uds_item_added(handle, item);
		if(r == UDS_ERROR_DISCONNECTED) {
			reinit();
			if(!handle) return -1;
		}
	}

	if(r == UDS_SUCCESS) return 0;
	return -2;
}

Q_DECL_EXPORT long long int hub_find_account_id(const char *username) {
	bb::pim::account::Account account;
	bb::pim::account::AccountService service;

	QList<bb::pim::account::Account> accounts = service.accounts();
	if(accounts.isEmpty()) return -3;

	bool found = false;
	for(QList<bb::pim::account::Account>::ConstIterator it = accounts.constBegin(); it != accounts.constEnd(); it++) {
		if(it->isExternalData() && it->externalSetupInvokeTarget() == owner && it->settingsProperty("user") == username) {
			if(found) {
				int r = 0;
				if((r = hub_remove_account(it->id())) < 0) return r;
			} else {
				found = true;
				account = *it;
			}
		}
	}

	if(found) {
		account.setExternalData(true);
		account.setSettingsValue("user", username);
		account.setExternalSetupInvokeTarget(owner);
		bb::pim::account::Result r = service.updateAccount(account.id(), account);
		if(!r.isSuccess()) return -4;
	} else {
		bb::pim::account::Provider external = service.provider("external");
		account = bb::pim::account::Account(external);
		account.setExternalData(true);
		account.setSettingsValue("user", username);
		account.setExternalSetupInvokeTarget(owner);
		bb::pim::account::Result r = service.createAccount(external.id(), account);
		if(!r.isSuccess()) return -4;
	}

	return account.id();
}

Q_DECL_EXPORT long long int hub_setup_account(const char *username, const char *displayName, const char *icon) {
	long long int accountId = hub_find_account_id(username);

	if(accountId < 0) return accountId;

	uds_account_data_t *account_data = uds_account_data_create();
	uds_account_data_set_id(account_data, accountId);
	uds_account_data_set_type(account_data, UDS_ACCOUNT_TYPE_IM);
	uds_account_data_set_name(account_data, displayName);
	uds_account_data_set_description(account_data, username);
	uds_account_data_set_icon(account_data, icon);
	uds_account_data_set_target_name(account_data, owner);

	int r = hub_update_account(account_data);

	uds_account_data_destroy(account_data);

	if(r == 0) return accountId;
	return r;
}

}
