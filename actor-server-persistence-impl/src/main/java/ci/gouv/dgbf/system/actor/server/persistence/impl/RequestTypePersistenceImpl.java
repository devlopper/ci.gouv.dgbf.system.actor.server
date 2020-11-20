package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.RequestTypePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestType;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class RequestTypePersistenceImpl extends AbstractPersistenceEntityImpl<RequestType> implements RequestTypePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}