#include <QList>
#include <QtDebug>

#include <bb/pim/account/Result>
#include <bb/pim/account/Account>
#include <bb/pim/account/Provider>
#include <bb/pim/account/AccountService>

#include "bb/pim/unified/unified_data_source.h"

/* This code is not threadsafe -- only invoke the hub from one thread */

extern "C" {

static uds_context_t handle = NULL;

static void reinit(const char *owner, const char *assets) {
	if(handle) uds_close(&handle);
	handle = NULL;

	if(uds_init(&handle, false) != UDS_SUCCESS) {
		handle = NULL;
		qDebug() << "uds_init FAILED";
	} else {
		int r = UDS_ERROR_TIMEOUT;
		while(r == UDS_ERROR_TIMEOUT) {
			r = uds_register_client(handle, owner, "", assets);
			if(r == UDS_ERROR_DISCONNECTED) reinit(owner, assets);
		}

		if(r == UDS_SUCCESS) return;
		qDebug() << "uds_register_client FAILED with: " << r;
	}
}

Q_DECL_EXPORT int hub_remove_account(long long int accountId, const char *owner, const char *assets) {
	if(!handle) reinit(owner, assets);
	if(!handle) return -1;

	int r = UDS_ERROR_TIMEOUT;
	while(r == UDS_ERROR_TIMEOUT) {
		r = uds_account_removed(handle, accountId);
		if(r == UDS_ERROR_DISCONNECTED) {
			reinit(owner, assets);
			if(!handle) return -1;
		}
	}

	if(r != UDS_SUCCESS) return -2;
	bb::pim::account::AccountService service;
	service.deleteAccount(accountId);

	return 0;
}

static long long int getAccountId(const QString &username, const QString &displayName, const char *owner, const char *assets) {
	bb::pim::account::Account account;
	bb::pim::account::AccountService service;

	QList<bb::pim::account::Account> accounts = service.accounts();
	if(accounts.isEmpty()) return -3;

	bool found = false;
	for(QList<bb::pim::account::Account>::ConstIterator it = accounts.constBegin(); it != accounts.constEnd(); it++) {
		if(it->isExternalData() && it->externalSetupInvokeTarget() == owner && it->settingsProperty("user") == username) {
			if(found) {
				int r = 0;
				if((r = hub_remove_account(it->id(), owner, assets)) < 0) return r;
			} else {
				found = true;
				account = *it;
			}
		}
	}

	if(found) {
		account.setExternalData(true);
		account.setDisplayName(displayName);
		account.setSettingsValue("user", username);
		account.setExternalSetupInvokeTarget(owner);
		bb::pim::account::Result r = service.updateAccount(account.id(), account);
		if(!r.isSuccess()) {
			qDebug() << "updateAccount FAILED: " << r.message();
			return -4;
		}
	} else {
		bb::pim::account::Provider external = service.provider("external");
		account = bb::pim::account::Account(external);
		account.setExternalData(true);
		account.setDisplayName(displayName);
		account.setSettingsValue("user", username);
		account.setExternalSetupInvokeTarget(owner);
		bb::pim::account::Result r = service.createAccount(external.id(), account);
		if(!r.isSuccess()) {
			qDebug() << "createAccount FAILED: " << r.message();
			return -4;
		}
	}

	return account.id();
}

Q_DECL_EXPORT int hub_setup_account(const char *username, const char *displayName, const char *icon, const char *owner, const char *assets) {
	int r = 0;
	long long int accountId = getAccountId(username, displayName, owner, assets);

	if(accountId < 0) return accountId;

	uds_account_data_t *account_data = uds_account_data_create();
	uds_account_data_set_id(account_data, accountId);
	uds_account_data_set_type(account_data, UDS_ACCOUNT_TYPE_IM);
	uds_account_data_set_name(account_data, displayName);
	uds_account_data_set_description(account_data, username);
	uds_account_data_set_icon(account_data, icon);
	uds_account_data_set_target_name(account_data, owner);

	if(!handle) reinit(owner, assets);
	if(!handle) {
		r = -1;
		goto cleanup;
	}

	r = UDS_ERROR_TIMEOUT;
	while(r == UDS_ERROR_TIMEOUT) {
		r = uds_account_updated(handle, account_data);
		if(r == UDS_ERROR_DISCONNECTED) {
			reinit(owner, assets);
			if(!handle) {
				r = -1;
				goto cleanup;
			}
		}
	}

	if(r == UDS_SUCCESS) goto cleanup;

	r = UDS_ERROR_TIMEOUT;
	while(r == UDS_ERROR_TIMEOUT) {
		r = uds_account_added(handle, account_data);
		if(r == UDS_ERROR_DISCONNECTED) {
			reinit(owner, assets);
			if(!handle) {
				r = -1;
				goto cleanup;
			}
		}
	}

	if(r != UDS_SUCCESS) r = -2;

cleanup:
	uds_account_data_destroy(account_data);

	if(r == UDS_SUCCESS) return accountId;
	return r;
}

}
