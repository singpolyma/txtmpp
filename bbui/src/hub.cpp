#include <QList>
#include <QtDebug>

#include <bb/pim/account/Result>
#include <bb/pim/account/Account>
#include <bb/pim/account/Provider>
#include <bb/pim/account/AccountService>

#include "bb/pim/unified/unified_data_source.h"

/* This code is not threadsafe -- only invoke the hub from one thread */

//extern "C" {

static uds_context_t handle = NULL;

static void reinit(const QString &owner, const QString &assets) {
	if(handle) uds_close(&handle);
	handle = NULL;

	if(uds_init(&handle, false) != UDS_SUCCESS) {
		handle = NULL;
		qDebug() << "uds_init FAILED";
	} else {
		int r = UDS_ERROR_TIMEOUT;
		while(r == UDS_ERROR_TIMEOUT) {
			r = uds_register_client(handle, owner.toUtf8().constData(), "", assets.toUtf8().constData());
			if(r == UDS_ERROR_DISCONNECTED) reinit(owner, assets);
		}

		if(r == UDS_SUCCESS) return;
		qDebug() << "uds_register_client FAILED with: " << r;
	}
}

static long long int getAccountId(const QString &username, const QString &displayName, const QString &owner, const QString &assets) {
	bb::pim::account::Account account;
	bb::pim::account::AccountService service;

	QList<bb::pim::account::Account> accounts = service.accounts();
	if(accounts.isEmpty()) return -2;

	bool found = false;
	for(QList<bb::pim::account::Account>::ConstIterator it = accounts.constBegin(); it != accounts.constEnd(); it++) {
		if(it->isExternalData() && it->externalSetupInvokeTarget() == owner && it->settingsProperty("user") == username) {
			if(found) {
				if(!handle) reinit(owner, assets);
				if(!handle) return -1;

				int r = UDS_ERROR_TIMEOUT;
				while(r == UDS_ERROR_TIMEOUT) {
					r = uds_account_removed(handle, it->id());
					if(r == UDS_ERROR_DISCONNECTED) {
						reinit(owner, assets);
						if(!handle) return -1;
					}
				}

				if(r != UDS_SUCCESS) return -3;
				service.deleteAccount(it->id());
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

int hub_setup_one_account(const QString &username, const QString &displayName, const QString &icon, const QString &owner, const QString &assets) {
	long long int accountId = getAccountId(username, displayName, owner, assets);

	if(accountId < 0) return accountId;

	QByteArray displayNameBytes = displayName.toUtf8();
	QByteArray usernameBytes = username.toUtf8();
	QByteArray iconBytes = icon.toUtf8();
	QByteArray ownerBytes = owner.toUtf8();

	uds_account_data_t *account_data = uds_account_data_create();
	uds_account_data_set_id(account_data, accountId);
	uds_account_data_set_type(account_data, UDS_ACCOUNT_TYPE_IM);
	uds_account_data_set_name(account_data, displayNameBytes.constData());
	uds_account_data_set_description(account_data, usernameBytes.constData());
	uds_account_data_set_icon(account_data, iconBytes.constData());
	uds_account_data_set_target_name(account_data, ownerBytes.constData());

	if(!handle) reinit(owner, assets);
	if(!handle) return -1;

	int r = UDS_ERROR_TIMEOUT;
	while(r == UDS_ERROR_TIMEOUT) {
		r = uds_account_updated(handle, account_data);
		if(r == UDS_ERROR_DISCONNECTED) {
			reinit(owner, assets);
			if(!handle) return -1;
		}
	}

	if(r == UDS_SUCCESS) return accountId;

	r = UDS_ERROR_TIMEOUT;
	while(r == UDS_ERROR_TIMEOUT) {
		r = uds_account_added(handle, account_data);
		if(r == UDS_ERROR_DISCONNECTED) {
			reinit(owner, assets);
			if(!handle) return -1;
		}
	}

	if(r == UDS_SUCCESS) return accountId;
	return -2;
}

//}
