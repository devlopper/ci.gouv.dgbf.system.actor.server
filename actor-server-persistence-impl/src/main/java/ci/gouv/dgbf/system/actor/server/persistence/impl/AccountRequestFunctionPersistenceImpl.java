package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.AccountRequestFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequestFunction;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class AccountRequestFunctionPersistenceImpl extends AbstractPersistenceEntityImpl<AccountRequestFunction> implements AccountRequestFunctionPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}