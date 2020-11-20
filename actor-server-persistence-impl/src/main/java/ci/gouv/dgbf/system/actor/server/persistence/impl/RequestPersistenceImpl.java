package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.RequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class RequestPersistenceImpl extends AbstractPersistenceEntityImpl<Request> implements RequestPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}