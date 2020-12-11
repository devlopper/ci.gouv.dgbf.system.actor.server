package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.RequestScopeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class RequestScopeFunctionPersistenceImpl extends AbstractPersistenceEntityImpl<RequestScopeFunction> implements RequestScopeFunctionPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}