package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.LocalityBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.LocalityPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class LocalityBusinessImpl extends AbstractBusinessEntityImpl<Locality, LocalityPersistence> implements LocalityBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
