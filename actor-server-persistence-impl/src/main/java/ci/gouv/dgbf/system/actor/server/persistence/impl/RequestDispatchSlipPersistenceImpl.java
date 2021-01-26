package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.RequestDispatchSlipPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class RequestDispatchSlipPersistenceImpl extends AbstractPersistenceEntityImpl<RequestDispatchSlip> implements RequestDispatchSlipPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}