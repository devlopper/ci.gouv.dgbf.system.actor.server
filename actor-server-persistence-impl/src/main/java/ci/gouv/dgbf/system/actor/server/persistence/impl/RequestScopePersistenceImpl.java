package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.RequestScopePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScope;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class RequestScopePersistenceImpl extends AbstractPersistenceEntityImpl<RequestScope> implements RequestScopePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}