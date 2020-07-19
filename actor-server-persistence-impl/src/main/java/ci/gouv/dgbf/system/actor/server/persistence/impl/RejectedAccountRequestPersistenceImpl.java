package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.RejectedAccountRequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RejectedAccountRequest;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class RejectedAccountRequestPersistenceImpl extends AbstractPersistenceEntityImpl<RejectedAccountRequest> implements RejectedAccountRequestPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}