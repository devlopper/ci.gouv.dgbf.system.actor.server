package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.RequestStatusBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.RequestStatusPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class RequestStatusBusinessImpl extends AbstractBusinessEntityImpl<RequestStatus, RequestStatusPersistence> implements RequestStatusBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
