package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.RequestFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestFunction;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class RequestFunctionPersistenceImpl extends AbstractPersistenceEntityImpl<RequestFunction> implements RequestFunctionPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}