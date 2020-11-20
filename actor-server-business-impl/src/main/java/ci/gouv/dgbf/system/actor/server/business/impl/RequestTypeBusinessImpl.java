package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.RequestTypeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.RequestTypePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestType;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class RequestTypeBusinessImpl extends AbstractBusinessEntityImpl<RequestType, RequestTypePersistence> implements RequestTypeBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
