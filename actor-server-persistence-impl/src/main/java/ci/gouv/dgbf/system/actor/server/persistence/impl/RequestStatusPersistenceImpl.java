package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.RequestStatusPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class RequestStatusPersistenceImpl extends AbstractPersistenceEntityImpl<RequestStatus> implements RequestStatusPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}