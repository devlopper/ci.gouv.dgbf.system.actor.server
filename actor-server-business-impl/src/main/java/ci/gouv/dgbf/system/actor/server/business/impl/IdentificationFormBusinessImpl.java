package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.IdentificationFormBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.IdentificationFormPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationForm;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class IdentificationFormBusinessImpl extends AbstractBusinessEntityImpl<IdentificationForm, IdentificationFormPersistence> implements IdentificationFormBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
