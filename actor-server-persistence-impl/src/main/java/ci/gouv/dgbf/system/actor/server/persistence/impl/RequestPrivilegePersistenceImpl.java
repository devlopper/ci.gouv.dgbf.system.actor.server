package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.RequestPrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestPrivilege;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class RequestPrivilegePersistenceImpl extends AbstractPersistenceEntityImpl<RequestPrivilege> implements RequestPrivilegePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}